This repository is aimed to provide an R function that allows matching individuals from a data frame, e.g., cases a controls of the same age, sex, etc.
</BR>


In the 'ParticipNonParticip.R' file, you will find an example of how to use the function 'matching_v03.r' using the data 'datExample.csv'. Note that:

* The function  'matching_v03.r' is located in the 'functions' subfolder

* The data  'datExample.csv' is located in the 'data' subfolder

</BR>
</BR>

The function’s arguments are:

**data**: names of the R dataFrame

**name.cascon**: variable to distinguish cases and controls

**case.value**: value assigned to cases in the ‘name.cascon’

**id.name**: variable that identifies each individual (no duplicates!)

**num.controls**: ratio controls vs. cases

**var.match**: a character vector with the names of the variables to be matched

**tol**: a numeric vector with the tolerance for each variable in the var.match (with exactly the same order)

**name.pair**: name for the variable that identifies the matched pairs

**seed.cases**: seed for cases

**seed.controls**: seed for control

</BR>
</BR>

Don't hesitate to contact me for further details, 

</BR>
</BR>
</BR>

Joan Vila</BR>
MSc in statistics</BR>
joanviladomenech@gmail.com
