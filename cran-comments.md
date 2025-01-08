## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new version of the package. 

* Corrected a rare overflow error in `rztpois` where an explicit cast from `double` to `int` was used -- now we use an explicit cast to `int` wrapped in `safe_double_to_int` to avoid overflow.  This overflow resulted in a segmentation fault in x86 hardware and in silent failures in AArch64 hardware. This bug should not be relevant for most practical uses of the package.  

* Added a new citation for the paper by Trikalinos and Sereda (2024, <doi:10.1371/journal.pone.0311311>)

* Some tests were added to check the behavior of the package on edge cases in x86 and AArch64 hardware. 

