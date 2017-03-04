from setuptools import setup

setup(name='pyaugur',
      version='0.1',
      description='AugurV2',
      url='',
      author='Daniel Eachern Huang',
      author_email='dan.e.huang@gmail.com',
      license='apache-2.0',
      packages=['pyaugur'],
      install_requires=[
          'numpy >= 1.12.0',
          'scipy >= 0.18.1',
          'pyyaml >= 3.11'
      ],
      zip_safe=False)
