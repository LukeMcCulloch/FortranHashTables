!!
!! Basic Hash Table
!!   maps character keys to character values
!!
!!   uncomment "real" and comment corresponding "character"
!!   to make a character to real hash table
!!

MODULE hash_in2r_class
  IMPLICIT NONE
  


  type, public :: hash_int2r
     INTEGER                            :: CharLength = 128
     INTEGER                            :: start_hash_size = 50
     INTEGER                            :: current_size, new_size
     INTEGER                            :: hash_index
     integer,               ALLOCATABLE :: keys(:)
     REAL,                  ALLOCATABLE :: values(:,:,:)
      LOGICAL,               ALLOCATABLE :: used(:)
    contains
      procedure :: hash_push
      procedure :: hash_set
      procedure :: hash_get
      procedure :: hash_print
      procedure :: hash_reallocate
      procedure :: print_error 
   end type hash_int2r



   interface hash_int2r
      module procedure hash_init
   end interface hash_int2r




 CONTAINS



   function hash_init()
     type(hash_int2r)                          hash_init
     integer :: status
     ALLOCATE(hash_init%keys(hash_init%start_hash_size),   stat=status)
     ALLOCATE(hash_init%values  (hash_init%start_hash_size,4,3)  , stat=status)
    ALLOCATE(hash_init%used(hash_init%start_hash_size),   stat=status)
    hash_init%hash_index = 0
    hash_init%keys(:)    = 0
    hash_init%values(:,:,:)= 0.0
    hash_init%used(:)    = .FALSE.
    hash_init%current_size = hash_init%start_hash_size
  end function hash_init


 


  SUBROUTINE hash_push(self, key, value)
    class(hash_int2r), intent(inout) :: self 
    integer,      INTENT(IN)     :: key
    REAL        , INTENT(IN)     :: value(4,3)
    self%hash_index = self%hash_index + 1
    IF(self%hash_index > Size(self%keys, 1)) CALL self%hash_reallocate
    self%keys(self%hash_index) = key
    self%values(self%hash_index,:,:) = value
    self%used(self%hash_index) = .TRUE.
  END SUBROUTINE hash_push
 




  SUBROUTINE hash_set(self, key, value)
    class(hash_int2r), intent(inout)      :: self 
    integer,       intent(in)              :: key
    real,          intent(in)              :: value(4,3)
    INTEGER                                :: local_index
    LOGICAL                                :: found
    found = .FALSE.
    DO local_index = 1,Size(self%keys,1)
      IF(self%keys(local_index) == key) THEN
        self%values(local_index,:,:) = value
        found = .TRUE.
      ENDIF
    ENDDO
    IF(.NOT.found) THEN
      call self%hash_push( key, value )
    ENDIF
  END SUBROUTINE hash_set
 




  SUBROUTINE hash_get(self, key, value)
    class(hash_int2r), intent(inout) :: self 
    integer,      intent(in)          :: key
    real,         intent(out)         :: value(4,3)
    INTEGER                           :: local_index
    LOGICAL                           :: found
    found = .FALSE.
    DO local_index = 1,Size(self%keys,1)
      IF(self%keys(local_index) == key) THEN
        value(:,:) = self%values(local_index,:,:)
        found = .TRUE.
      ENDIF
    ENDDO
    IF(.NOT.found) call self%print_error("Unknown key")
  END SUBROUTINE hash_get


!!$  function hash_return(self, key) result(value)
!!$    class(hash_int2ch), intent(inout) :: self 
!!$    integer,      intent(in)   :: key
!!$    CHARACTER(len=128)               :: value
!!$    INTEGER                    :: local_index
!!$    LOGICAL                    :: found
!!$    found = .FALSE.
!!$    DO local_index = 1,Size(self%keys,1)
!!$      IF(self%keys(local_index) == key) THEN
!!$        value = self%values(local_index)
!!$        found = .TRUE.
!!$      ENDIF
!!$    ENDDO
!!$    IF(.NOT.found) call self%print_error("Unknown key")
!!$    return
!!$  END function hash_return





 
  SUBROUTINE hash_print(self)
    class(hash_int2r), intent(inout) :: self 
    INTEGER  :: local_index, i
    print*,''
    PRINT*, "Entire Contents of the hashtable:"
    DO local_index = 1,Size(self%keys,1)
       IF(self%used(local_index)) then
          PRINT*, 'Quad',self%keys(local_index),'='
          do i=1,4
            print*, self%values(local_index,i,:)
         end do
      end if
      
    ENDDO
    print*, ''
  END SUBROUTINE hash_print
 



  SUBROUTINE hash_reallocate(self)
    class(hash_int2r), intent(inout) :: self
    integer,                    ALLOCATABLE :: temp_keys(:) 
    real,                       ALLOCATABLE :: temp_values(:,:,:)  
    LOGICAL                   , ALLOCATABLE :: temp_used(:)
    INTEGER                                 :: status
    self%new_size = self%current_size + self%start_hash_size
    ALLOCATE(temp_keys(self%current_size))
    ALLOCATE(temp_values(self%current_size,4,3))
    ALLOCATE(temp_used(self%current_size))
    temp_keys(:) = self%keys
    temp_values(:,:,:) = self%values(:,:,:)
    temp_used(:) = self%used(:)
    DEALLOCATE(self%keys)
    DEALLOCATE(self%values)
    DEALLOCATE(self%used)
    ALLOCATE(self%keys(self%new_size))
    self%keys(:) = ""
    ALLOCATE(self%values(self%new_size,4,3))
    self%values(:,:,:) = ""
    ALLOCATE(self%used(self%new_size))
    self%used(:) = .FALSE.
    self%keys(1:self%current_size) = temp_keys(:)
    self%values(1:self%current_size,:,:) = temp_values(:,:,:)
    self%used(1:self%current_size) = temp_used(:)
  END SUBROUTINE hash_reallocate
 




  SUBROUTINE print_error(self, text)
    class(hash_int2r), intent(inout) :: self 
    CHARACTER(*) :: text
    PRINT*, text
    STOP
  END SUBROUTINE print_error




END MODULE hash_in2r_class

