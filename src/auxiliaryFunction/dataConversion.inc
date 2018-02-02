    !!!- Converts an integer to a string
    CHARACTER(LEN=CHAR_LEN) FUNCTION  int2str(int)
        !CHARACTER(LEN=*) :: int2str
        INTEGER          :: int
        WRITE(int2str,*) int
        int2str = TRIM(ADJUSTL(int2str))
    END FUNCTION int2str

    !!!- Converts a string to an integer
    Integer FUNCTION str2int(lstr)
        CHARACTER*(*) :: lstr
        INTEGER       :: result,leng,i,strtoint
        leng   = len(lstr)
        result = 0
        DO i=1,leng
            IF (ichar(lstr(i:i))<58 .and.ichar(lstr(i:i))>=48) THEN
                result = 10*result+(ichar(lstr(i:i))-48)
            ENDIF
        ENDDO
        str2int = result
    END FUNCTION str2int
