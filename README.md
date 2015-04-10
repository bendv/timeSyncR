# News

#### April 10, 2015
- ```ggplot``` option in ```tsChipsRGB()``` replaced by ```plot```, which plots zoo objects instead of using ggplot2
- ability to export RGB chips as a list of raster bricks added (```exportChips```)
- ability to export centre pixel time series as a list of zoo objects added (```exportZoo```)
- [tutorial](http://bendv.github.io/timeSyncR) updated

# timeSyncR

#### Summary
Tools to aid visualization and interpretation of Landsat time series in R for calibration/validation of change detection methods. This package is (loosely) based on the <a href="http://timesync.forestry.oregonstate.edu/index.html" target="_blank">TimeSync</a> method (Cohen et al., 2010).

#### Installation
```
library(devtools)
install_github('bendv/timeSyncR')
library(timeSyncR)
```

#### Implementation
Check out this [short tutorial](http://bendv.github.io/timeSyncR). This is very much a work in progress...

#### References
Cohen, W. B., Yang, Z., & Kennedy, R. (2010). Detecting trends in forest disturbance and recovery using yearly Landsat time series: 2. TimeSync - Tools for calibration and validation. Remote Sensing of Environment, 114(12), 2911–2924. <a href="http://dx.doi.org/10.1016/j.rse.2010.07.010" target="_blank">doi:10.1016/j.rse.2010.07.010</a>
