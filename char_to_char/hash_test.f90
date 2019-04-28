PROGRAM hash_test
  use hash_chtch_class



  character(len=128)::res
 
  type(hash_chtch)   :: h1,h2

  h1 = hash_chtch()
 
  print*, '----'
  CALL h1%hash_set("one",  '1.0')
  CALL h1%hash_set("two",  '2.0')
  CALL h1%hash_set("three",'3.0')
 
  print*, '----'
  CALL h1%hash_print
  CALL h1%hash_get("one", res)
  PRINT*, res
  CALL h1%hash_get("two", res)
  PRINT*, res
  CALL h1%hash_get("three", res)
  PRINT*, res
 

 
  !print*, '----'
  !CALL h1%hash_set("one", '60.0')
  !CALL h1%hash_print
  !CALL h1%hash_get("one", res)
  !PRINT*, res
END PROGRAM hash_test
