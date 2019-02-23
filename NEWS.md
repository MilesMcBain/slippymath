# slippymath 0.3.0

Many function name changes in preparation for CRAN submisson. *These will certainly break existing `slippymath` code*. 

I have gone with a more explicit API since I feel users will mainly use
`slippymath` under the hood as a dependency. Therefore it is important they can
understand the API quickly rather than type it quickly - since they won't do
that very often.

Function names changing:
* `bb_tile_extent` -> `bbox_tile_extent`
* `bb_tile_query` -> `bbox_tile_query`
* `bb_to_tg` -> `bbox_to_tile_grid`
* `tg_composite` -> `compose_tile_grid`
* `tile_bb` -> `tile_bbox`
* `tg_bbs` -> `tile_grid_bboxes`


# slippymath 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* BREAKING CHANGE. There were mixed lat/lon lon/lat conventions. I have settled on the lon/lat ordering. This means the following functions have changed name and argument order: 
    - `latlon_to_tilenum` -> `lonlat_to_tilenum`
    - `tilenum_to_latlon`  -> `tilenum_to_lonlat`

