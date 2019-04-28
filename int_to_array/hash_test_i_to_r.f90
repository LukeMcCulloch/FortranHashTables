PROGRAM hash_test
  use hash_in2r_class


  REAL, dimension(4,3) :: inp, res
  integer              :: i,j
  !integer :: res = 1
  !character(len=128)::res
 
  type(hash_int2r)   :: h1,h2

  
  h1 = hash_int2r()
 
  print*, '----'
  
  
  
  
  inp(:,:)=0.
  print*, inp(1,:)
  print*, inp(2,:)
  print*, inp(3,:)
  print*, inp(4,:) 
  CALL h1%hash_set(1,  inp)
  inp(:,:) = 0.0
  inp(1,:) = (/1.,2.,3./)
  CALL h1%hash_set(2,  inp)
  inp(:,:) = 0.0
  inp(2,:) = (/1.,2.,3./)
  CALL h1%hash_set(3,  inp)
  inp(:,:) = 0.0
  inp(3,:) = (/1.,2.,3./)
  call h1%hash_set(4, inp)
  inp(:,:) = 0.0
  inp(4,:) = (/1.,2.,3./)
  call h1%hash_set(5, inp)  
 

  print*,'get the quads, asequentially'
  print*,''

  CALL h1%hash_get(2, res)
  do i=1,4
     print*, 'quad 2 = ',res(i,:)
  end do
  print*,''

  CALL h1%hash_get(4, res)
  do i=1,4
     print*, 'quad 4 = ',res(i,:)
  end do
  print*,''

  CALL h1%hash_get(3, res)
  do i=1,4
     print*, 'quad 3 = ',res(i,:)
  end do
  print*,''

  CALL h1%hash_get(5, res)
  do i=1,4
     print*, 'quad 5 = ',res(i,:)
  end do
  print*,''
  CALL h1%hash_get(1, res)


  print*, '----'
  CALL h1%hash_print
  

  !CALL h1%hash_get(2, res)
  !PRINT*, res
  !CALL h1%hash_get(3, res)
  !PRINT*, res
 
  !print*,'try the function way:'
  !h1%hash_return(3)
  !res = call h1%hash_return(3)
 
  !print*, '----'
  !CALL h1%hash_set("one", '60.0')
  !CALL h1%hash_print
  !CALL h1%hash_get("one", res)
  !PRINT*, res
END PROGRAM hash_test
