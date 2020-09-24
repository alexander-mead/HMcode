from setuptools import setup, find_packages, Extension
from distutils.command.install import install as _install
from distutils.command.clean import clean as _clean
from setuptools.command.build_py import build_py as _build_py    
from setuptools.command.develop import develop as _develop

import subprocess

import os
import sys
import shutil
import glob

HERE = os.path.abspath(os.path.dirname(__file__))

os.makedirs(os.path.join(HERE, "f90wrap_pyhmcode/pyhmcode"), exist_ok=True)

def compile_library(env):
    subprocess.check_call(["make", "f90wrap"], env=env, cwd=HERE)
    LIB = glob.glob("f90wrap_pyhmcode/_pyhmcode*.so")
    shutil.copy(LIB[0], "f90wrap_pyhmcode/pyhmcode")

def clean_library(env={}):
    subprocess.check_call(["make", "clean"], env=env, cwd=HERE)

class build(_build_py):
    def run(self):
        env = os.environ
        compile_library(env)
        super().run()

class develop(_develop):
    def run(self):
        env = os.environ
        compile_library(env)
        super().run()

class install(_install):
    def __init__(self, dist):
        super().__init__(dist)
        self.build_args = {}
        if self.record is None:
            self.record = "install-record.txt"

    def run(self):
        super().run()

class clean(_clean):
    def run(self):
        clean_library()
        super().run()

setup(name = "pyhmcode",
      description       = "Python interface for HMCode",
      author            = "Tilman Troester",
      author_email      = "tilmantroester@gmail.com",
      package_dir       = {"" : "f90wrap_pyhmcode/"},
      packages          = ["pyhmcode"],
      package_data      = {"" : ["_pyhmcode*.so"]},
      ext_modules       = [Extension('pyhmcode._pyhmcode', [])],
      install_requires  = ['numpy', 'f90wrap'],
      cmdclass={"install"   : install,
                "develop"   : develop,
                "build_py"  : build,
                "clean"     : clean},
        )

