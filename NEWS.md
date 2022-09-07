# Version 0.3.4

## Bug fix
- Minor fix (`format(..., digits = 0)` now causes error, replacing with `digists = 1`)


# Version 0.3.3

## Bug fix
- Minor fix (required after dependency updates)


# Version 0.3.2

## Bug fixes
- Add exception if a negative variogram range is fitted
- Another small fix


# Version 0.3.1

- Demo mode was improved ad corrected.
- Bug fixing (using single border polygon).


# Version 0.3.0

- Package was "stabilised" to be correctly exported on different systems
    (`guinterp_ui()` and `guinterp_server()` are now internal functions,
    so that functions used by them are correctly imported).
- A demo mode was added (in demo mode the user cannot load only two example files,
    the minimum resolution is 25m and the output raster can not be exported).
- An English demo app was built at https://ranghetti.shinyapps.io/guinterp/
    and linked to the online documentation.


# Version 0.2.5 (pre-release)

- Use the sill instead of the partial sill in variogram definition
- Use `leafpm` instead of `leaflet.extras`
- Accessory functions are no more exported
- Document package


# Version 0.2.4 (pre-release)

- Add summary statistics below histogram
- Manage statistics / map sample sizes separately


# Version 0.2.3 (pre-release)

- Add English translation using a multi-language interface (system locale is used)
- Add two new filters (manual selection and select by polygon)
- Manage the possibility to subsample points both in visualisation and in processing
- Manage different subsampling schemes among polygons
- Bug fixes


# Version 0.2.2 (pre-release)

Bug fixing


# Version 0.2.1

Change kriging algorithm (#5)


# Version 0.2.0 (pre-release)

First working version
