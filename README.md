# Land cover over time in the Lake Washington watershed

This uses imagery from the National Land Cover Database (NLCD) to measure land cover and impervious surface in the Lake Washington watershed (Seattle metro area, USA). NLCD images are available from 2001-2021, specifically: 2001, 2004, 2006, 2008, 2011, 2013, 2016, 2019, and 2021. 

"Land cover.R" reads in each year's imagery and crops and masks it to the watershed boundary. It calculates the percent of the watershed land area in each land-cover category as well as in impervious surface. The area classified as "open water" changed slightly in different years, so for consistency all years' totals are divided by the 2021 area of land in the watershed. 

The script produces two outputs: a full table of percent cover in each land-cover category each year, and a summary table where some categories have been grouped together (e.g., combining evergreen, deciduous, and mixed forest into a single forest category).
