# Stdlib imports
import os

# Local package imports
from . import ast_transforms as transforms
from . import clang_parser


def gen_c_ast(header_path, include_dirs, language):
    """ Parse the given header file into a C style ast which can be
    transformed into a CWrap ast. The include dirs are passed along to 
    gccxml.

    """
    c_ast = clang_parser.parse(header_path, include_dirs, language)
    return c_ast


def generate_asts(config):
    """ Returns an iterable of ASTContainer objects.

    """
    c_ast_containers = []
    for header_file in config.files:
        # read the header info and create the extern and implemenation
        # module names
        path = header_file.path
        header_name = os.path.split(path)[-1]
        extern_name = header_file.metadata.get('extern_name')
        implementation_name = header_file.metadata.get('implementation_name')
        if extern_name is None:
            extern_name = '_' + os.path.splitext(header_name)[0]
        if implementation_name is None:
            implementation_name = os.path.splitext(header_name)[0]

        # generate the c_ast for the header 
        include_dirs = config.metadata.get('include_dirs', [])

        language = config.metadata.get('language', '')

        print 'Parsing %s' % path
        ast_items = gen_c_ast(path, include_dirs, language)

        # Apply the transformations to the ast items 
        trans_items = transforms.apply_c_ast_transformations(ast_items)

        # Create the CAstContainer for these items
        container = transforms.CAstContainer(trans_items, header_name,
                                             extern_name, implementation_name)

        # Add the container to the list
        c_ast_containers.append(container)

    # Now we can create an ast transformer and transform the list 
    # of containers into a generator that can be rendered into code
    ast_transformer = transforms.CAstTransformer(c_ast_containers)
    return ast_transformer.transform()
