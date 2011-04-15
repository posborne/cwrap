from collections import defaultdict
import os

from cStringIO import StringIO
import cy_ast


UNDEFINED = '__UNDEFINED__'


CODE_HEADER = """\
# This code was automatically generated by CWrap.

"""


class Code(object):

    def __init__(self):
        self._io = StringIO()
        self._indent_level = 0
        self._indentor = '    '
        self._imports = defaultdict(set)
        self._imports_from = defaultdict(lambda: defaultdict(set))
        self._cimports = defaultdict(set)
        self._cimports_from = defaultdict(lambda: defaultdict(set))

    def indent(self, n=1):
        self._indent_level += n

    def dedent(self, n=1):
        self._indent_level -= n

    def write_i(self, code):
        indent = self._indentor * self._indent_level
        self._io.write('%s%s' % (indent, code))

    def write(self, code):
        self._io.write(code)

    def add_import(self, module, as_name=None):
        self._imports[module].add(as_name)

    def add_import_from(self, module, imp_name, as_name=None):
        self._imports_from[module][imp_name].add(as_name)

    def add_cimport(self, module, as_name=None):
        self._cimports[module].add(as_name)
    
    def add_cimport_from(self, module, imp_name, as_name=None):
        if as_name is not None:
            self._cimports_from[module][imp_name].add(as_name)
        else:
            self._cimports_from[module][imp_name]

    def _gen_imports(self):
        import_lines = []

        # cimports
        cimport_items = sorted( self._cimports.iteritems() )
        for module, as_names in cimport_items:
            if as_names:
                for name in sorted(as_names):
                    import_lines.append('cimport %s as %s' % (module, name))
            else:
                import_lines.append('cimport %s' % module)
        
        if import_lines:
            import_lines.append('\n')

        # cimports from
        cimport_from_items = sorted( self._cimports_from.iteritems() )
        for module, impl_dct in cimport_from_items:
            sub_lines = []
            for impl_name, as_names in sorted(impl_dct.iteritems()):
                if as_names:
                    for name in sorted(as_names):
                        sub_lines.append('%s as %s' % (impl_name, name))
                else:
                    sub_lines.append(impl_name)
            sub_txt = ', '.join(sub_lines)
            import_lines.append('from %s cimport %s' % (module, sub_txt))

        if import_lines:
            import_lines.append('\n')

        # cimports
        import_items = sorted( self._imports.iteritems() )
        for module, as_names in import_items:
            if as_names:
                for name in sorted(as_names):
                    import_lines.append('import %s as %s' % (module, name))
            else:
                import_lines.append('import %s' % module)

        if import_lines:
            import_lines.append('\n')

        # cimports from
        import_from_items = sorted( self._imports_from.iteritems() )
        for module, impl_dct in import_from_items:
            sub_lines = []
            for impl_name, as_names in sorted(impl_dct.iteritems()):
                if as_names:
                    for name in sorted(as_names):
                        sub_lines.append('%s as %s' % (impl_name, name))
                else:
                    sub_lines.append(impl_name)
            sub_txt = ', '.join(sub_lines)
            import_lines.append('from %s import %s' % (module, sub_txt))

        return '\n'.join(import_lines)
                    
    def code(self):
        imports = self._gen_imports()
        if imports:
            res = CODE_HEADER + imports + '\n\n' + self._io.getvalue()
        else:
            res = CODE_HEADER + self._io.getvalue()
        return res


class ExternRenderer(object):

    def __init__(self):
        self.context = [None]
        self.code = None

    def render(self, items, header_name):
        self.context = [None]
        self.code = Code()

        self.code.write_i('cdef extern from "%s":\n\n' % header_name)
        self.code.indent()
        for item in items:
            self.visit(item)
        self.code.dedent()

        return self.code.code()
     
    def visit(self, node):
        self.context.append(node)
        method = 'visit_' + node.__class__.__name__
        visitor = getattr(self, method, self.unhandled_node)
        visitor(node)
        self.context.pop()

    def unhandled_node(self, node):
        print 'Unhandled node in extern renderer: `%s`' % node
    
    def visit_Typedef(self, typedef):
        if isinstance(typedef.typ, (cy_ast.Struct, cy_ast.Enumeration, cy_ast.Union)):
            #self.visit(typedef.typ)
            self.code.write_i('ctypedef %s %s\n\n' % (typedef.typ.name, typedef.name))
        elif isinstance(typedef.typ, (cy_ast.PointerType, cy_ast.ArrayType)):
            name = typedef.name
            self.code.write_i('ctypedef ')
            self.visit(typedef.typ)
            self.code.write(' %s\n\n' % name)
        elif isinstance(typedef.typ, (cy_ast.Typedef, cy_ast.FundamentalType)):
            name = typedef.name
            typ_name = typedef.typ.name
            self.code.write_i('ctypedef %s %s\n\n' % (typ_name, name))
        else:
            print 'Unhandled typedef type in extern renderer: `%s`' % typedef.typ

    def visit_Struct(self, struct):
        name = struct.name
        if struct.opaque:
            self.code.write_i('cdef struct %s\n' % name)
        else:
            self.code.write_i('cdef struct %s:\n' % name)
            self.code.indent()
            for field in struct.members:
                self.visit(field)
            self.code.dedent()
        self.code.write('\n')
    
    def visit_Union(self, union):
        name = union.name
        if union.opaque:
            self.code.write_i('cdef union %s\n' % name)
        else:
            self.code.write_i('cdef union %s:\n' % name)
            self.code.indent()
            for field in union.members:
                self.visit(field)
            self.code.dedent()
        self.code.write('\n')

    def visit_Field(self, field):
        name = field.name
        if isinstance(field.typ, (cy_ast.Typedef, cy_ast.FundamentalType,
                                  cy_ast.Struct, cy_ast.Enumeration)):
            typ_name = field.typ.name
        elif isinstance(field.typ, (cy_ast.PointerType, cy_ast.ArrayType)):
            typ_name, name = self.apply_modifier(field.typ, name)
        else:
            print 'Unhandled field type in extern renderer: `%s`.' % field.typ
            typ_name = UNDEFINED
        self.code.write_i('%s %s\n' % (typ_name, name))
   
    def visit_Enumeration(self, enum):
        name = enum.name
        if enum.opaque:
            if name is None:
                self.code.write_i('cdef enum\n')
            else:
                self.code.write_i('cdef enum %s\n' % name)
        else:
            if name is None:
                self.code.write_i('cdef enum:\n')
            else:
                self.code.write_i('cdef enum %s:\n' % name)
            self.code.indent()
            for value in enum.values:
                self.visit(value)
            self.code.dedent()
        self.code.write('\n')
   
    def visit_EnumValue(self, enum_value):
        name = enum_value.name
        self.code.write_i('%s\n' % name)
    
    def visit_Function(self, function):
        name = function.name
        returns = function.returns

        if isinstance(returns, (cy_ast.Typedef, cy_ast.FundamentalType, cy_ast.Enumeration)):
            self.code.write_i('%s ' % returns.name)
        elif isinstance(returns, cy_ast.PointerType):
            res_name, name = self.apply_modifier(returns, name)
            self.code.write_i('%s ' % res_name)
        else:
            print 'undhandled return function type node: `%s`' % returns
            self.code.write_i('%s\n\n' % UNDEFINED)
            return 
        
        self.code.write('%s(' % name)

        if len(function.arguments) == 0:
            self.code.write(')\n\n')
            return

        if len(function.arguments) == 1:
            if isinstance(function.arguments[0].typ, cy_ast.FundamentalType):
                if function.arguments[0].typ.name == 'void':
                    self.code.write(')\n\n')
                    return
        
        for arg in function.arguments[:-1]:
            self.visit(arg)
            self.code.write(', ')
        self.visit(function.arguments[-1])

        self.code.write(')\n\n')

    def visit_Argument(self, argument):
        name = argument.name
        typ = argument.typ
        if isinstance(typ, (cy_ast.Typedef, cy_ast.FundamentalType, 
                            cy_ast.Enumeration, cy_ast.Struct)):
            typ_name = typ.name
        elif isinstance(typ, (cy_ast.PointerType, cy_ast.ArrayType)):
            typ_name, name = self.apply_modifier(typ, name)
        elif isinstance(typ, cy_ast.CvQualifiedType):
            typ_name = typ.name
        else:
            print 'unhandled argument type node: `%s`' % typ
            typ_name = UNDEFINED
        if name is not None:
            self.code.write('%s %s' % (typ_name, name))
        else:
            self.code.write('%s' % typ_name)

    def visit_PointerType(self, pointer):
        self.visit(pointer.typ)

    def visit_ArrayType(self, array):
        self.visit(array.typ)
       
    def visit_Ignored(self, node):
        pass

    def visit_CvQualifiedType(self, node):
        pass

    def visit_FunctionType(self, node):
        pass

    def visit_OperatorFunction(self, node):
        pass

    def visit_Macro(self, node):
        pass

    def visit_Alias(self, node):
        pass

    def visit_File(self, node):
        pass
   
    def visit_Variable(self, var):
        name = var.name
        if isinstance(var.typ, (cy_ast.Typedef, cy_ast.FundamentalType)):
            self.code.write_i('%s %s\n\n' % (var.typ.name, name))
        else:
            print 'unhandled variable typ: `%s`' % var.typ
            self.code.write_i(UNDEFINED + '\n\n')

    def apply_modifier(self, node, name):
        stack = []
        typ = node
        
        while isinstance(typ, (cy_ast.PointerType, cy_ast.ArrayType,
                               cy_ast.CvQualifiedType)):
            if isinstance(typ, (cy_ast.PointerType, cy_ast.ArrayType)):
                stack.append(typ)
            typ = typ.typ
        
        if isinstance(typ, (cy_ast.Typedef, cy_ast.FundamentalType, 
                            cy_ast.Struct)):
            typ_name = typ.name
        else:
            print 'Unhandled typ node in apply_modifier: `%s`.' % typ
            return UNDEFINED, UNDEFINED
        
        for i, node in enumerate(stack):
            if i > 0:
                if not isinstance(node, type(stack[i - 1])):
                    name = '(' + name + ')'
            
            if isinstance(node, cy_ast.PointerType):
                name = '*' + name
            elif isinstance(node, cy_ast.ArrayType):
                max = node.max
                if max is None:
                    dim = ''
                else:
                    dim = str(max + 1)
                name = name + ('[%s]' % dim)  
            else:
                print 'Unhandled node in apply_notifier: `%s`.' % node
                return UNDEFINED, UNDEFINED

        return typ_name, name


