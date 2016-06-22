# CDR distribution 

This Python code takes (1) Milano Cell-tower grid file (GeoJSON) and (2) CDR data as input, and outputs a GeoJSON file having properties of (1) cell-in, (2) cell-out, (3) sms-in, (4) sms-out, and (5) internet activities. 

Since the code is using Cython, you can compile "pyx" code first by running...

```sh
python setup.py build_ext --inplace
```

Then, you can run a Python code that calls to the C code.

```python
python cython_test.py
```

Still, need some more speed-up to deal with all the datasets.