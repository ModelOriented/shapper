shapper 0.1.4
----------------------------------------------------------------
* `plot.individual_variable_effect` does not interrupt too early
  due to the unexpected vecotr conversion (#39)

shapper 0.1.3
----------------------------------------------------------------
* `id` param for `plot.individual_variable_effect` now works as intended. Thanks (#27)
* Removed duplicated functions from `DALEX`
* `shapper` now imports `DALEX`

shapper 0.1.2
----------------------------------------------------------------
* #12 issue fix
* #13 issue exception for column mismatch added 
* added tryCatch to .onLoad to prevent packages crashes when shap is not available

shapper 0.1.1
----------------------------------------------------------------
* Support for conda envs in install_shap() added. 

shapper 0.1
----------------------------------------------------------------
* shapper package is now public
