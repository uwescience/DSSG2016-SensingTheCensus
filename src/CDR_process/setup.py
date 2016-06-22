from distutils.core import setup
from Cython.Build import cythonize

setup(
  name = 'loop for CDR',
  ext_modules = cythonize("process.pyx"),
)