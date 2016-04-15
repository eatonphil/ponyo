# TODO

#### ponyo-test
* test runner will parse a signature that maps to a structure of tests
  * signature will enumerate all functions to be tested
    * functions must be unit -> bool
  * test runner will build a test script that calls each function and captures
    failure
  * tests will be executed concurrently by default

#### ponyo-make
* should properly guess main-file if directory is provided
  * currently stubbed out
* ponyo-make doesn't fail correctly
  * fail in Makefile doesn't trigger end of build attempts

#### ponyo lib
* make sure occurences of "^" are sensible
* ensure tail-call recursion is used everywhere
* build out test suite
  * setup automated testing on repo
* add min/max functions to list
* fix list sortBy stub
* add time module with standard time formats
  * rfc3339/iso8601

#### ponyo-doc
* docs should include date generated