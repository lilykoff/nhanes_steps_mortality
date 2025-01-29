## Overview

The project files include:

-   `subject-info.csv`: NHANES SEQN (id), age, and gender for all subjects 

-   13 files, each named `nhanes_1440_<varname>.csv.xz`

-   `<varname>` is one of: `actisteps`, `adeptsteps`, `oaksteps`, `scrfsteps`, `scsslsteps`, `vssteps`, `vsrevsteps`, `AC`, `log10AC`, `PAXMTSM`, `log10PAXMTSM`, `PAXPREDM`, `PAXFLGSM`

-   Each row in each file is one day for one participant. Each file contains the following columns: - `SEQN`: NHANES participant ID, a character scalar

-   `PAXDAYM`: NHANES day of physical activity measurements for the participant, integer between 1 and 9. Note: days 1 and 9 will not have complete data.

-   `PAXDAYWM`: day of the week, integer between 1 and 7, where 1 corresponds to Sunday, 2 to Monday, ..., and 7 to Saturday.

-   `min_x` for `x = 1, 2, ..., 1440`: the value of `<varname>` for minute `x`. For `actisteps`, `adeptsteps`, `oaksteps`, `scrfsteps`, `scsslsteps`, `vssteps`, `vsrevsteps`, `AC`, `log10AC`, `PAXMTSM`, `log10PAXMTSM`, the values are floats. For `PAXPREDM` they are integers, where `1` = wake wear, `2` = sleep wear, `3` = unknown wear, and `4` = nonwear. For `PAXFLGSM` the values are logical, where `TRUE` corresponds to any wear flags and `FALSE` corresponds to no wear flags.


## **Key**

-   `actisteps` = Actilife steps

-   `oaksteps` = Oak steps

-   `scrfsteps` = stepcount random forest steps

-   `scsslsteps` = stepcount SSL steps

-    `vssteps` = Verisense steps

-   `vsrevsteps` = Verisense revised steps

-   `AC` = ActiGraph activity counts

-   `log10AC` = log10 of 1 + activity counts

-   `PAXMTSM` = MIMS

-   `log10PAXMTSM` = log10 of 1 + MIMS

-   `PAXPREDM` = wear prediction

-   `PAXFLGSM` = wear flags
