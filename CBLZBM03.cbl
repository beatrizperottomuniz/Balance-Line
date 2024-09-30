       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLZBM03.
      ******************************************************************
      * Author: BEATRIZ PEROTTO MUNIZ
      * Date: 19-01-24
      * Purpose: BALANCE LINE , GERAR RELATORIO COM EMPRESA E SEUS SÓCIOS
      * Tectonics: cobc
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT SOCIOS ASSIGN TO
       "C:\Users\adria\Downloads\SOCIOS.txt"
       FILE STATUS IS AS-STATUS-S.

       SELECT LISTA ASSIGN TO
       "C:\Users\adria\Downloads\CNPJ.txt"
       FILE STATUS IS AS-STATUS-L.

       SELECT RELATORIO ASSIGN TO
       "C:\Users\adria\Downloads\BEATRIZPM.txt"
       FILE STATUS IS AS-STATUS-R.

       DATA DIVISION.
       FILE SECTION.

       FD SOCIOS
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 ARQ-SOCIOS                     PIC X(71).
       01 FILLER REDEFINES ARQ-SOCIOS.
           05 ARQ-S-CNPJ                 PIC 9(014).
           05 ARQ-S-CNPJ-SOCIO           PIC 9(014).
           05 ARQ-S-NOME                 PIC X(036).
           05 ARQ-S-STATUS               PIC X(005).
           05 ARQ-S-FIM                  PIC X(002).


       FD LISTA
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 ARQ-LISTA                     PIC X(96).
       01 FILLER REDEFINES ARQ-LISTA.
           05 ARQ-L-CNPJ                PIC 9(014).
           05 ARQ-L-SIT                 PIC X(006).
           05 ARQ-L-NOME                PIC X(059).
           05 ARQ-L-ATIVO               PIC X(005).
           05 ARQ-L-DATA-ABERT          PIC X(010).
           05 ARQ-L-FIM                 PIC X(002).

       FD RELATORIO
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.
      * PARA WORD
       01 ARQ-RELATORIO                PIC X(73).


       WORKING-STORAGE SECTION.
       01 AS-STATUS-S                  PIC 9(02) VALUE ZEROS.
       01 AS-STATUS-L                  PIC 9(02) VALUE ZEROS.
       01 AS-STATUS-R                  PIC 9(02) VALUE ZEROS.

      *---------------------------------------------------------
      *     DEFINICAO DE CABECALHO
      *---------------------------------------------------------
       01 WS-CABEC-REL1                PIC X(60) VALUE ALL '='.

       01 WS-CABEC-REL2.
           05 WS-CABEC-REL2-PGM        PIC X(08) VALUE 'CBLZBM03'.
           05 WS-CABEC-REL2-FL1        PIC X(13) VALUE SPACES.
           05 WS-CABEC-REL2-DES        PIC X(09) VALUE 'VOLVO S.A'.
           05 WS-CABEC-REL2-FL2        PIC X(20) VALUE SPACES.
           05 WS-CABEC-REL2-DT         PIC X(10) VALUE SPACES.

       01 WS-CABEC-REL3.
           05 WS-CABEC-REL3-HR         PIC X(08) VALUE SPACES.
           05 WS-CABEC-REL3-FL1        PIC X(10) VALUE SPACE.
           05 WS-CABEC-REL3-DES        PIC X(23) VALUE
                                       'BALANCE LINE'.
      *---------------------------------------------------------
      *     DEFINICAO DE DETALHE
      *---------------------------------------------------------
       01 LINDET01-REL.
           05 LINDET01-REL-CNPJ        PIC X(04) VALUE 'CNPJ'.
           05 LINDET01-REL-SPACE2      PIC X(19) VALUE SPACE.
           05 LINDET01-REL-NOME        PIC X(08) VALUE 'NOME'.
           05 LINDET01-REL-SPACE3      PIC X(30) VALUE SPACE.

       01 LINDET02-REL.
           05 LINDET02-REL-CNPJ        PIC X(10) VALUE 'CNPJ SOCIO'.
           05 LINDET02-REL-SPACE2      PIC X(17) VALUE SPACE.
           05 LINDET02-REL-NOME        PIC X(10) VALUE 'NOME SOCIO'.
           05 LINDET02-REL-SPACE3      PIC X(30) VALUE SPACE.

      *para imprimir as empresas
       01 LINDET03-REL.
           05 LINDET03-REL-CNPJ        PIC X(14) VALUE SPACES.
           05 LINDET03-REL-SPACE2      PIC X(09) VALUE SPACES.
           05 LINDET03-REL-NOME        PIC X(40) VALUE SPACES.
           05 LINDET03-REL-SPACE3      PIC X(30) VALUE SPACES.

      *para imprimir os socios
       01 LINDET04-REL.
           05 LINDET04-REL-CNPJ        PIC X(14) VALUE SPACES.
           05 LINDET04-REL-SPACE2      PIC X(13) VALUE SPACES.
           05 LINDET04-REL-NOME        PIC X(40) VALUE SPACES.
           05 LINDET04-REL-SPACE3      PIC X(30) VALUE SPACES.


       01 AS-FIM-S                       PIC X(01) VALUE 'N'.
       01 AS-FIM-L                       PIC X(01) VALUE 'N'.
      *VARIAVEL PARA EVITAR WRITE DUPLO DE EMPRESA
       01 AS-PRI-S                       PIC X(01) VALUE 'S'.
      *-----------------------------------------------------------
      *HORA
      *-----------------------------------------------------------
       01  WS-DATA PIC X(08) VALUE SPACES.
       01  WS-HORA PIC X(06) VALUE SPACES.

       PROCEDURE DIVISION.

            PERFORM 1000-INICIALIZAR.
            IF AS-FIM-L EQUALS 'N' AND AS-FIM-S EQUALS 'N'
               PERFORM 2000-PROCESSAR UNTIL AS-FIM-S = 'S'
            END-IF.
            PERFORM 3000-FINALIZAR.


      *---------------------------------------------------------
      *     INCIALIZACAO
      *---------------------------------------------------------
       1000-INICIALIZAR                SECTION.

      *FAZ COM QUE HORA E DATA SEJAM ATUALIZADAS

           ACCEPT WS-DATA FROM DATE YYYYMMDD.
           ACCEPT WS-HORA FROM TIME.

           MOVE WS-DATA(1:4)           TO WS-CABEC-REL2-DT(7:4)
           MOVE WS-DATA(5:2)           TO WS-CABEC-REL2-DT(4:2)
           MOVE WS-DATA(7:2)           TO WS-CABEC-REL2-DT(1:2)
           MOVE '/'                    TO WS-CABEC-REL2-DT(3:1)
                                          WS-CABEC-REL2-DT(6:1)

           MOVE WS-HORA(1:2)           TO WS-CABEC-REL3-HR(1:2)
           MOVE WS-HORA(3:2)           TO WS-CABEC-REL3-HR(4:2)
           MOVE WS-HORA(5:2)           TO WS-CABEC-REL3-HR(7:2)
           MOVE ':'                    TO WS-CABEC-REL3-HR(3:1)

      *ABRE ARQUIVOS

           OPEN INPUT SOCIOS.
           IF AS-STATUS-S NOT EQUAL ZEROS
               DISPLAY 'DEU ERRO NA ABERTURA ' AS-STATUS-S
           END-IF

           OPEN INPUT LISTA.
           IF AS-STATUS-L NOT EQUAL ZEROS
               DISPLAY 'DEU ERRO NA ABERTURA ' AS-STATUS-L
           END-IF

           OPEN OUTPUT RELATORIO.
           IF AS-STATUS-R NOT EQUAL ZEROS
               DISPLAY 'DEU ERRO NA ABERTURA ' AS-STATUS-R
           END-IF

      *MONTANDO O CABEÇALHO
           MOVE WS-CABEC-REL1          TO ARQ-RELATORIO
           WRITE ARQ-RELATORIO.
           IF AS-STATUS-R NOT EQUAL ZEROS
               DISPLAY 'DEU ERRO NA ESCRITA ' AS-STATUS-R
           END-IF
           MOVE WS-CABEC-REL2          TO ARQ-RELATORIO
           WRITE ARQ-RELATORIO.
           IF AS-STATUS-R NOT EQUAL ZEROS
               DISPLAY 'DEU ERRO NA ESCRITA ' AS-STATUS-R
           END-IF
           MOVE WS-CABEC-REL3          TO ARQ-RELATORIO
           WRITE ARQ-RELATORIO.
           IF AS-STATUS-R NOT EQUAL ZEROS
               DISPLAY 'DEU ERRO NA ESCRITA ' AS-STATUS-R
           END-IF
           MOVE WS-CABEC-REL1          TO ARQ-RELATORIO
           WRITE ARQ-RELATORIO.
           IF AS-STATUS-R NOT EQUAL ZEROS
               DISPLAY 'DEU ERRO NA ESCRITA ' AS-STATUS-R
           END-IF
           MOVE LINDET01-REL           TO ARQ-RELATORIO
           WRITE ARQ-RELATORIO.
           IF AS-STATUS-R NOT EQUAL ZEROS
               DISPLAY 'DEU ERRO NA ESCRITA ' AS-STATUS-R
           END-IF
           MOVE LINDET02-REL           TO ARQ-RELATORIO
           WRITE ARQ-RELATORIO.
           IF AS-STATUS-R NOT EQUAL ZEROS
               DISPLAY 'DEU ERRO NA ESCRITA ' AS-STATUS-R
           END-IF


           READ LISTA.
           IF AS-STATUS-L NOT EQUAL ZEROS
               DISPLAY 'ARQUIVO VAZIO '
               MOVE 'S'                TO AS-FIM-L
           END-IF
           .

           READ SOCIOS.
           IF AS-STATUS-S NOT EQUAL ZEROS
               DISPLAY 'ARQUIVO VAZIO '
               MOVE 'S'                TO AS-FIM-S
           END-IF
           .

       1000-INICIALIZAR-EXIT.
           EXIT.
      *---------------------------------------------------------
      *     PROCESSAMENTO
      *---------------------------------------------------------
       2000-PROCESSAR                  SECTION.

           EVALUATE TRUE
               WHEN ARQ-L-CNPJ EQUALS ARQ-S-CNPJ
      *ESSA CONDICAO VE SE PRECISA IMPRIMIR A EMPRESA (SE FOR O SEGUNDO SOCIO NAO IMPRIME)
                   IF AS-PRI-S EQUALS 'S'
                       MOVE 'N'                    TO AS-PRI-S
                       MOVE ARQ-L-CNPJ             TO LINDET03-REL-CNPJ
                       MOVE ARQ-L-NOME             TO LINDET03-REL-NOME

                   MOVE LINDET03-REL           TO ARQ-RELATORIO
                   WRITE ARQ-RELATORIO
                   IF AS-STATUS-R NOT EQUAL ZEROS
                       DISPLAY 'DEU ERRO NA ESCRITA ' AS-STATUS-R
                   END-IF
                   END-IF

                   MOVE ARQ-S-CNPJ-SOCIO       TO LINDET04-REL-CNPJ
                   MOVE ARQ-S-NOME             TO LINDET04-REL-NOME

                   MOVE LINDET04-REL           TO ARQ-RELATORIO
                   WRITE ARQ-RELATORIO
                   IF AS-STATUS-R NOT EQUAL ZEROS
                       DISPLAY 'DEU ERRO NA ESCRITA ' AS-STATUS-R
                   END-IF

                   READ SOCIOS
                   IF AS-STATUS-S NOT EQUAL ZEROS
                       MOVE 'S'                TO AS-FIM-S
                   END-IF

               WHEN ARQ-L-CNPJ > ARQ-S-CNPJ
                   IF AS-PRI-S EQUALS 'N'
                       MOVE 'S'                        TO AS-PRI-S
                   END-IF
                   READ SOCIOS
                   IF AS-STATUS-S NOT EQUAL ZEROS
                       MOVE 'S'                TO AS-FIM-S
                   END-IF
               WHEN ARQ-L-CNPJ < ARQ-S-CNPJ
                   IF AS-PRI-S EQUALS 'N'
                       MOVE 'S'                        TO AS-PRI-S
                   END-IF
                   READ LISTA
                   IF AS-STATUS-S NOT EQUAL ZEROS
                       MOVE 'S'                TO AS-FIM-S
                   END-IF

           END-EVALUATE

           .
       2000-PROCESSAR-EXIT.
           EXIT.


       3000-FINALIZAR                  SECTION.

      *fechando
           CLOSE SOCIOS
           IF AS-STATUS-S NOT EQUAL ZEROS
               DISPLAY 'DEU ERRO NO FECHAR ' AS-STATUS-S
           END-IF

           CLOSE LISTA
           IF AS-STATUS-L NOT EQUAL ZEROS
               DISPLAY 'DEU ERRO NO FECHAR ' AS-STATUS-L
           END-IF

           CLOSE RELATORIO
           IF AS-STATUS-R NOT EQUAL ZEROS
               DISPLAY 'DEU ERRO NO FECHAR ' AS-STATUS-R
           END-IF

            STOP RUN.
       3000-FINALIZAR-EXIT.
           EXIT.
       END PROGRAM CBLZBM03.
