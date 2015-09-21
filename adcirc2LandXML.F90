!
    PROGRAM ADC2LandXML
        IMPLICIT NONE

        CHARACTER(200)                  :: inGridFile
        CHARACTER(200)                  :: outXMLFile
        CHARACTER(300)                  :: line,line2,line3
        CHARACTER(9)                    :: tempC
        CHARACTER(100)                  :: strX,strY,strZ
        CHARACTER(10)                   :: N1,N2,N3
        INTEGER                         :: I
        INTEGER                         :: tempI, tempI2
        INTEGER                         :: NP,NE
        INTEGER,ALLOCATABLE             :: conn(:,:)
        REAL(8),ALLOCATABLE             :: nodes(:,:)

        WRITE(*,'(A,$)') "Name of ADCIRC mesh file: "
        READ(*,*) inGridFile
        !inGridFile = "53K.14"

        WRITE(*,'(A,$)') "Name of output XML file: "
        READ(*,*) outXMLFile
        !outXMLFile = "temp.xml"

        OPEN(UNIT=14,FILE=TRIM(inGridFile),ACTION='READ')
        READ(14,*)
        READ(14,*)NE,NP
        ALLOCATE(nodes(3,NP))
        ALLOCATE(conn(3,NE))
        DO I = 1, NP
            READ(14,*)tempI,nodes(1,I),nodes(2,I),nodes(3,I)
            IF (I.NE.tempI) THEN
                WRITE(*,'(A)') "Mesh needs renumbered..."
                STOP
            ENDIF
            !nodes(1,I) = ABS(nodes(1,I))
        ENDDO
        DO I = 1, NE
            READ(14,*)tempI,tempI2,conn(1,I),conn(2,I),conn(3,I)
            IF (I.NE.tempI) THEN
                WRITE(*,'(A)') "Mesh needs renumbered..."
                STOP
            ENDIF
        ENDDO
        CLOSE(14)

        OPEN(UNIT=10,FILE=TRIM(outXMLFile),STATUS='UNKNOWN')
        WRITE(10,'(A)') "<?xml version=""1.0"" ?>"
        line = "<LandXML version=""1.2"" xmlns=""http://www.landxml.org/schema/LandXML-1.2"""
        line2 = " xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xsi:schemaLocation="
        line3 = """http://www.landxml.org/schema/LandXML-1.2 http://www.landxml.org/schema/LandXML-1.2/LandXML-1.2.xsd"">"
        WRITE(10,'(A)') TRIM(line)//TRIM(line2)//TRIM(line3)


        WRITE(10,'(2x,A)') "<Surfaces>"
        WRITE(10,'(4x,A)') "<Surface names=""My TIN"">"
        WRITE(10,'(6x,A)') "<Definition surfType=""TIN"">"
        WRITE(10,'(8x,A)') "<Pnts>"
        DO I = 1, NP
            WRITE(tempC,'(I9)') I
            tempC = ADJUSTL(tempC)
            WRITE(strX,'(F15.8)') nodes(2,I)
            strX = ADJUSTL(strX)
            WRITE(strY,'(F15.8)') nodes(1,I)
            strY = ADJUSTL(strY)
            WRITE(strZ,'(F15.8)') nodes(3,I)
            strZ = ADJUSTL(strZ)
            WRITE(10,'(10x,A)') "<P id="""//TRIM(tempC)//""">"//TRIM(strX)//" "//TRIM(strY)//" "//TRIM(strZ)//"</P>"
        ENDDO
        WRITE(10,'(8x,A)') "</Pnts>"
        WRITE(10,'(8x,A)') "<Faces>"
        DO I = 1, NE
            WRITE(tempC,'(I9)') I
            tempC = ADJUSTL(tempC)
            WRITE(N1,'(I9)') conn(1,I)
            N1 = ADJUSTL(N1)
            WRITE(N2,'(I9)') conn(2,I)
            N2 = ADJUSTL(N2)
            WRITE(N3,'(I9)') conn(3,I)
            N3 = ADJUSTL(N3)
            WRITE(10,'(10x,A)') "<F id="""//TRIM(tempC)//""">"//TRIM(N1)//" "//TRIM(N2)//" "//TRIM(N3)//"</F>"
        ENDDO
        WRITE(10,'(8x,A)') "</Faces>"
        WRITE(10,'(6x,A)') "</Definition>"
        WRITE(10,'(4x,A)') "</Surface>"
        WRITE(10,'(2x,A)') "</Surfaces>"
        WRITE(10,'(A)') "</LandXML>"

        CLOSE(10)

    END PROGRAM

