PROGRAM hash_test
  use hash_in2ch_class


  !REAL :: res = 0.0
  !integer :: res = 1
  character(len=128)::res
 
  type(hash_int2ch)   :: h1,h2

  h1 = hash_int2ch()
 
  print*, '----'
  CALL h1%hash_set(1,  'EN')
  CALL h1%hash_set(2,  'METRIC KG')
  CALL h1%hash_set(3,  'METRIC KN')
 
  print*, '----'
  CALL h1%hash_print
  CALL h1%hash_get(1, res)
  PRINT*, res
  CALL h1%hash_get(2, res)
  PRINT*, res
  CALL h1%hash_get(3, res)
  PRINT*, res
 
  !print*,'try the function way:'
  !h1%hash_return(3)
  !res = call h1%hash_return(3)
 
  !print*, '----'
  !CALL h1%hash_set("one", '60.0')
  !CALL h1%hash_print
  !CALL h1%hash_get("one", res)
  !PRINT*, res
END PROGRAM hash_test
