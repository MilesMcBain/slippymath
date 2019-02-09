# slippymath 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* BREAKING CHANGE. There were mixed lat/lon lon/lat conventions. I have settled on the lon/lat ordering. This means the following functions have changed name and argument order: 
    - `latlon_to_tilenum` -> `lonlat_to_tilenum`
    - `tilenum_to_latlon`  -> `tilenum_to_lonlat`
