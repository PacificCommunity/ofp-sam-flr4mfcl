# FLR4MFCL

requires FLCore 

FLR4MFCL - R4MFCL built with FLR classes
Copyright (C) 2018  Rob Scott

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
    
Installation

FLR4MFCL depends on FLCore (FLR-project) which itself depends on iterators.
```{r}
install.packages("iterators")
install.packages("FLCore", repos="http://flr-project.org/R")
install.packages("remotes")

library(remotes)
install_github("robscott3/FLR4MFCL")
```

NOTE: Installation from github may fail due to warning messages being elevated to errors. You can override this behaviour by setting the environment variable as follows and re-running the install_github command - but use with caution because some warnings may not be completely benign!
```{r}
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
```
