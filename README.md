h5store
=======

Store arbitrary lists of array-like objects as HDF5
objects. The library handles the dimension names.

Objects are saved in HDF5 groups of `value` with their actual content and
`names_<1..n>` for their dimension names. Upon loading they are restored in
their original state.

### Installation

The package requires a modified version of the rhdf5 library and devtools to
install package from Github.

```r
devtools::install_github("mschubert/rhdf5")
devtools::install_github("mschubert/r-h5store")
```

### Usage

`h5save(object, file_name)` saves an object the the file `file_name`.

`h5load(file_name, path)` loads an object from `file_name`, optionally only
subsetting `path`.
