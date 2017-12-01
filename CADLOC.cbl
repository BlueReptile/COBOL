       Program-Id. CADLOC.

      *=========================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           Special-names.
           Decimal-point is comma.

      *=========================================================================*
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              SELECT ARQ-LOC ASSIGN TO DISK
              ORGANIZATION INDEXED
              ACCESS MODE DYNAMIC
              RECORD KEY RECKEY
              ALTERNATE RECORD KEY DESC-LOCAL WITH DUPLICATES
              FILE STATUS ARQST.

      *=========================================================================*
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-LOC LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "LOCAL.DAT".
           01  REG-LOCAL.
               02 RECKEY.
                   03 CODIGO-LOCAL          PIC 9(04).
               02 DESC-LOCAL           PIC X(30).
               02 AREA-LOCAL            PIC 9(05).
               02 CODIGO-DEPTO          PIC 9(04).

      *=========================================================================*
       WORKING-STORAGE SECTION.

           01 REG-LOCAL-E.
               02 RECKEY-E.
                   03 CODIGO-LOCAL-E    PIC 9(04).
               02 DESC-LOCAL-E           PIC X(30).
               02 AREA-LOCAL-E           PIC 9(05).
               02 CODIGO-DEPTO-E         PIC 9(04).

           01 DATA-SIS.
               02 ANO                      PIC 9(04).
               02 MES                      PIC 9(02).
               02 DIA                      PIC 9(02).

           01 DESMES.
               02 FILLER                   PIC X(10) VALUE "JANEIRO".
               02 FILLER                   PIC X(10) VALUE "FEVEREIRO".
               02 FILLER                   PIC X(10) VALUE "MARÇO".
               02 FILLER                   PIC X(10) VALUE "ABRIL".
               02 FILLER                   PIC X(10) VALUE "MAIO".
               02 FILLER                   PIC X(10) VALUE "JUNHO".
               02 FILLER                   PIC X(10) VALUE "JULHO".
               02 FILLER                   PIC X(10) VALUE "AGOSTO".
               02 FILLER                   PIC X(10) VALUE "SETEMBRO".
               02 FILLER                   PIC X(10) VALUE "OUTUBRO".
               02 FILLER                   PIC X(10) VALUE "NOVEMBRO".
               02 FILLER                   PIC X(10) VALUE "DEZEMBRO".

           01 TABELA-MESES REDEFINES DESMES.
               02 MES-T                    PIC X(10) OCCURS 12 TIMES.


           01 ARQST                        PIC X(02).
           01 OP                           PIC X(01) VALUE SPACES.
           01 SALVA                        PIC X(01) VALUE SPACES.
           01 WIGUAL                       PIC 9     VALUE ZEROS.
           01 ESPACO                       PIC X(60) VALUE SPACES.
           01 OP-CONTINUA                  PIC X(1)  VALUE SPACES.

      *=========================================================================*
       SCREEN SECTION.

           01 TELA-MENU.
               02 BLANK SCREEN.
               02 LINE 02 COL 05 VALUE "SANTOS,    DE            DE.".
               02 LINE 02 COL 55 VALUE "SEMI-PARCAS CORP.".
               02 LINE 04 COL 29 VALUE "CONTROLE DE PATRIMNIO".
               02 LINE 10 COL 29 VALUE "MENU PRINCIPAL".
               02 LINE 12 COL 29 VALUE "[1] INCLUSAO DE LOCAL".
               02 LINE 13 COL 29 VALUE "[2] ALTERACAO DE LOCAL".
               02 LINE 14 COL 29 VALUE "[3] EXCLUSAO DE LOCAL".
               02 LINE 15 COL 29 VALUE "[4] CONSULTA POR CODIGO".
               02 LINE 16 COL 29 VALUE "[5] CONSULTA POR DESCRICAO".
               02 LINE 17 COL 29 VALUE "[6] SAIR".
               02 LINE 20 COL 29 VALUE "ESCOLHA UMA OPCAO [ ]".
               02 LINE 23 COL 11 VALUE "MENSAGEM:".


          01 TELA.
               02 BLANK SCREEN.
               02 LINE 02 COL 05 VALUE "SANTOS,    DE            DE  .".
               02 LINE 02 COL 55 VALUE "BATATAS CORP.".
               02 LINE 04 COL 29 VALUE "CONTROLE DE PATRIMONIO".
               02 LINE 10 COL 29 VALUE "CODLOCAL:".
               02 LINE 12 COL 29 VALUE "DESCRICAO:".
               02 LINE 14 COL 29 VALUE "AREA:".
               02 LINE 16 COL 29 VALUE "CODDEPTO:".
               02 LINE 23 COL 11 VALUE "MENSAGEM:".

      *=========================================================================*
       PROCEDURE DIVISION.

       Inicio.

           PERFORM ABRE-ARQ.
           PERFORM MENU UNTIL OP = "6".
           PERFORM finaliza.

      *-------------------------------------------------------------------------*
       Abre-arq.

           OPEN I-O ARQ-LOC.
           IF ARQST NOT = "00"
               DISPLAY "ERRO DE ABERTURA"
               STOP " "
               CLOSE ARQ-LOC
               OPEN OUTPUT ARQ-LOC
               CLOSE ARQ-LOC
               OPEN I-O ARQ-LOC.

      *-------------------------------------------------------------------------*
       Menu.

           DISPLAY erase at 0101.
           DISPLAY TELA-MENU.
           PERFORM MOSTRA-DATA.
           ACCEPT OP AT 2048.
           PERFORM TRATAR-OPCAO.

      *-------------------------------------------------------------------------*
       Mostra-data.

           MOVE FUNCTION CURRENT-DATE TO DATA-SIS.
           DISPLAY DIA AT 0213.
           DISPLAY MES-T(MES) AT 0219.
           DISPLAY ANO AT 0233.

      *-------------------------------------------------------------------------*
       Tratar-opcao.

           MOVE SPACES TO OP-CONTINUA
           EVALUATE OP
           WHEN "1"
               PERFORM INCLUSAO        UNTIL OP-CONTINUA = "N" OR "n"
           WHEN "2"
               PERFORM ALTERACAO       UNTIL OP-CONTINUA = "N" OR "n"
           WHEN "3"
               PERFORM EXCLUSAO        UNTIL OP-CONTINUA = "N" OR "n"
           WHEN "4"
               PERFORM CONSULTA-COD    UNTIL OP-CONTINUA = "N" OR "n"
           WHEN "5"
               PERFORM CONSULTA-DES    UNTIL OP-CONTINUA = "N" OR "n"
           END-EVALUATE.

      *-------------------------------------------------------------------------*
       finaliza.

            CLOSE ARQ-LOC.
            exit program.
      *-------------------------------------------------------------------------*

       Tela-entrada.

           DISPLAY erase at 0101.
           DISPLAY TELA.
           PERFORM MOSTRA-DATA.

      *-------------------------------------------------------------------------*
       Inicializar.

           MOVE SPACES TO OP
                          OP-CONTINUA
                          SALVA
                          DESC-LOCAl-E.
           MOVE ZEROS TO  AREA-LOCAL-E
                          CODIGO-LOCAL-E
                          CODIGO-DEPTO-E.
           DISPLAY ESPACO AT 2321.



      *-------------------------------------------------------------------------*
       Inclusao.

           PERFORM TELA-ENTRADA.
           DISPLAY "CADASTRO DE TIPOS" AT 0629 WITH HIGHLIGHT.
           MOVE ZEROS TO WIGUAL.
           PERFORM INICIALIZAR.
           PERFORM RECEBE-DADOS.
           PERFORM GRAVAR.
           PERFORM CONTINUA UNTIL OP-CONTINUA = "S" OR "N" OR "s"OR"n ".

      *-------------------------------------------------------------------------*
       Consulta-cod.
           PERFORM TELA-ENTRADA.
           DISPLAY "CONSULTA DE CODIGOS" AT 0629 WITH HIGHLIGHT.
           PERFORM LE-DADOS.
           PERFORM CONTINUA UNTIL OP-CONTINUA = "S" OR "N" OR "s" OR"n".

      *-------------------------------------------------------------------------*
       Consulta-des.

           PERFORM TELA-ENTRADA.
           DISPLAY "CONSULTA DE REGISTRO" AT 0629 WITH HIGHLIGHT.
           PERFORM LE-DADOS-DES.
           PERFORM CONTINUA UNTIL OP-CONTINUA = "S" OR "N" OR "s" OR"n".


      *-------------------------------------------------------------------------*
       Recebe-dados.

           MOVE 0 TO WIGUAL.
           PERFORM Recebe-codigo UNTIL WIGUAL = 1
           PERFORM Recebe-descricao  UNTIL WIGUAL = 0
           PERFORM Recebe-area  UNTIL WIGUAL = 1
           MOVE 0 TO WIGUAL
           PERFORM Recebe-depto UNTIL WIGUAL = 1.

      *-------------------------------------------------------------------------*
       Recebe-codigo.
           MOVE 0 TO WIGUAL.
           ACCEPT CODIGO-LOCAL-E AT 1040.
           IF CODIGO-LOCAL-E = 0 THEN
                DISPLAY "Codigo não pode ser nulo" AT 2321
           ELSE
                MOVE 1 TO WIGUAL
                MOVE CODIGO-LOCAL-E TO CODIGO-LOCAL
                READ ARQ-LOC NOT INVALID KEY PERFORM JA-CADASTRADO
                END-READ.
                DISPLAY CODIGO-LOCAL AT 1043.

      *-------------------------------------------------------------------------*
       Recebe-descricao.

           ACCEPT DESC-LOCAL-E AT 1240.
           IF DESC-LOCAL-E = SPACES THEN
                DISPLAY "DIGITE UMA DESCRIÇÃO MAIOR." AT 2321
           ELSE
                MOVE 0 TO WIGUAL
                MOVE DESC-LOCAL-E TO DESC-LOCAL
                DISPLAY ESPACO AT 2321.

      *-------------------------------------------------------------------------*
       Recebe-area.
           MOVE 0 TO WIGUAL.
           ACCEPT AREA-LOCAL-E AT 1440.
           IF AREA-LOCAL-E< 01 THEN
                DISPLAY "DIGITE O NUMERO DA AREA." AT 2321
           ELSE
                MOVE 1 TO WIGUAL.
                MOVE AREA-LOCAL-E TO AREA-LOCAL.
                DISPLAY ESPACO AT 2321.
      *-------------------------------------------------------------------------*
       Recebe-depto.
           MOVE 0 TO WIGUAL.
           ACCEPT CODIGO-DEPTO-E AT 1640.
           IF CODIGO-DEPTO-E < 01 THEN
                DISPLAY "DIGITE O NUMERO DO DEPTO." AT 2321
           ELSE
                MOVE 1 TO WIGUAL.
                MOVE CODIGO-DEPTO-E TO CODIGO-DEPTO.
                DISPLAY ESPACO AT 2321.
      *-------------------------------------------------------------------------*
       Gravar.

           DISPLAY "SALVAR <S/N> [ ]" AT 2321.
           ACCEPT SALVA AT 2335 WITH PROMPT AUTO.
           IF SALVA = "S" OR "s" THEN
               WRITE REG-LOCAL
               DISPLAY ARQST AT 2221
               STOP " ".

      *-------------------------------------------------------------------------*
       Continua.

           DISPLAY ESPACO AT 2321.
           DISPLAY "CONTINUA (S/N) [ ]" AT 2321.
           ACCEPT OP-CONTINUA AT 2337 WITH PROMPT AUTO.
           IF OP-CONTINUA = "S" OR "N" OR "s" OR "n"
                     DISPLAY ESPACO AT 2321
                     DISPLAY ESPACO AT 2421
              ELSE
                     DISPLAY ESPACO AT 2321
                     DISPLAY "DIGITE S OU N" AT 2321.

      *-------------------------------------------------------------------------*
       Ja-cadastrado.

           DISPLAY ESPACO AT 2321.
           DISPLAY "TIPO JA CADASTRADO" AT 2321.
           SET WIGUAL TO 1.

      *-------------------------------------------------------------------------*
       Le-dados.

           PERFORM INICIALIZAR.
           MOVE ZEROS TO WIGUAL.
           PERFORM Recebe-codigo UNTIL WIGUAL = 1.
           PERFORM Recebe-descricao.
           IF WIGUAL <> 1 THEN
              DISPLAY "NAO ENCONTRADO" AT 1440.
           IF ARQST = "00" THEN
              DISPLAY ESPACO AT 2321
              DISPLAY AREA-LOCAL AT 1043
              DISPLAY CODIGO-DEPTO AT 1440.

      *-------------------------------------------------------------------------*
       Le-dados-des.

           PERFORM INICIALIZAR.
           ACCEPT DESC-LOCAL-E AT 1440.
           MOVE DESC-LOCAL-E TO DESC-LOCAL.
           READ ARQ-LOC KEY IS DESC-LOCAL INVALID KEY
                DISPLAY "DESCRICAO NAO ENCONTRADA" AT 2321
                MOVE SPACES TO DESC-LOCAL.
                STOP " ".
           IF ARQST = "00" THEN
              DISPLAY ESPACO AT 2321
              DISPLAY CODIGO-DEPTO-E AT 1040.

      *-------------------------------------------------------------------------*
       Alteracao.

           PERFORM TELA-ENTRADA.
           DISPLAY "ALTERACAO DE REGISTRO" AT 0629 WITH HIGHLIGHT.
           PERFORM LE-DADOS.
           IF WIGUAL <> 1
             MOVE CODIGO-LOCAL TO CODIGO-LOCAL-E
             PERFORM RECEBE-DESCRICAO
             DISPLAY "SALVAR <S/N> [ ]" AT 2321
             ACCEPT SALVA AT 2335 WITH PROMPT AUTO
             IF SALVA = "S" OR "s" THEN
                 REWRITE REG-LOCAL
                 DISPLAY ESPACO AT 2321.
           PERFORM CONTINUA UNTIL OP-CONTINUA = "S" OR "N" OR "s" OR"n".

      *-------------------------------------------------------------------------*
       Exclusao.
           PERFORM TELA-ENTRADA.
           DISPLAY "EXCLUSAO DE REGISTRO" AT 0629 WITH HIGHLIGHT.
           PERFORM LE-DADOS.
           IF ARQST = "00" THEN
               DISPLAY "DESEJA EXCLUIR O REGISTRO <S/N> [ ]" AT 2321
               ACCEPT SALVA AT 2354 WITH PROMPT AUTO
           ELSE
               PERFORM INICIALIZAR
               DISPLAY ESPACO AT 2321
               DISPLAY "REGISTRO NAO ENCONTRADO." AT 2321.
           IF SALVA = "S" OR "s" THEN
               DISPLAY ESPACO AT 2321
               DISPLAY "REGISTRO APAGADO." AT 2321
               DELETE ARQ-LOC.
           STOP " ".
           DISPLAY ESPACO AT 2321.
           PERFORM CONTINUA UNTIL OP-CONTINUA = "S" OR "N" OR "s"OR"n".


      *=========================================================================*
