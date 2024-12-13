IDENTIFICATION DIVISION.
PROGRAM-ID. AOC02.

ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT IFILE 
                   ASSIGN TO FILENAME 
                   ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
       FILE SECTION.
           FD IFILE.
           01 FILE-LINE PICTURE IS X(80).
                   88 EOF VALUE HIGH-VALUES.

WORKING-STORAGE SECTION.
       01 FILENAME PICTURE IS X(80).
       01 VAL PICTURE IS 9(5).
       01 PREV PICTURE IS 9(5).
       01 GOING-DOWN PICTURE IS 9(5).
       01 DISTANCE PICTURE IS 9(5).
       01 DIRECTION PICTURE IS 9.
       01 N PICTURE IS 9(5).
       01 SAFE PICTURE IS 9(5).
       01 NSAFE PICTURE IS 9(5) VALUE IS ZERO.

PROCEDURE DIVISION.
       IF NUMBER-OF-CALL-PARAMETERS NOT EQUAL TO 1 THEN
           DISPLAY "usage: 02 FILENAME" UPON STDERR
           MOVE 2 TO RETURN-CODE 
           STOP RUN
       END-IF
   
       ACCEPT FILENAME FROM COMMAND-LINE
       OPEN INPUT IFILE

       PERFORM UNTIL EOF
            MOVE 1 TO N
            MOVE 1 TO SAFE

            READ IFILE
              AT END
                   SET EOF TO TRUE
              NOT AT END
                   PERFORM UNTIL FILE-LINE IS EQUAL TO SPACES OR SAFE IS EQUAL TO 0
                           *> adorable COBOL magic: replace the first space with '@'
                           INSPECT FILE-LINE REPLACING FIRST SPACE BY '@' 

                           *> now read the current number and put the rest of the line back
                           UNSTRING FILE-LINE DELIMITED BY '@' INTO VAL FILE-LINE

                           *> if N is 1, we don't have to do anything (yet)
                           IF N IS GREATER THAN 1 THEN
                              COMPUTE DISTANCE = FUNCTION ABS(VAL - PREV)

                              *> first test: if distance is zero or greater than 3, the sequence is not safe
                              IF DISTANCE IS EQUAL TO ZERO OR DISTANCE IS GREATER THAN 3 THEN
                                 MOVE 0 TO SAFE
                              END-IF

                              *> if N is 2, then we have to set the direction, too.
                              IF N IS EQUAL TO 2 THEN
                                 IF VAL < PREV THEN
                                    MOVE 1 TO GOING-DOWN
                                 ELSE
                                    MOVE 0 TO GOING-DOWN
                                 END-IF
                              ELSE
                                 *> we're at the third number or later, so we also have to check that we're still going 
                                 *> the right way
                                 IF GOING-DOWN IS EQUAL TO 1 AND VAL > PREV OR GOING-DOWN IS EQUAL TO 0 AND VAL < PREV THEN
                                    MOVE 0 TO SAFE
                                 END-IF
                              END-IF
                           END-IF
                           
                           MOVE VAL TO PREV
                           MOVE ZERO TO VAL
                           ADD 1 TO N
                   END-PERFORM

                   ADD SAFE TO NSAFE
            END-READ
       END-PERFORM

       DISPLAY 'Safe sequences: ' NSAFE

       CLOSE IFILE.
EXIT PROGRAM.
