      *TO-DO - Arrumar Tela Inclusao
      *Arrumar espaço "Continua S/N" as vezes não limpa o texto anterior
      *Implementar Consulta por Descricao

       Identification Division.
       Program-Id. MOVIM.

       Environment Division.
       CONFIGURATION SECTION.
       special-names.
       decimal-point is comma.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              SELECT MOVIM ASSIGN TO DISK
              ORGANIZATION INDEXED
              ACCESS MODE DYNAMIC
              RECORD KEY CODIGO-BEM
              ALTERNATE RECORD KEY CODIGO-LOCAL WITH DUPLICATES
              FILE STATUS ARQST.


       data division.
       file section.
       fd  MOVIM LABEL RECORD STANDARD
       value of file-id is "HISTOR.DAT  ".
       01 reg-his.
           02 Chaves.
               03 CODIGO-BEM     pic 9(06).
           02 CODIGO-LOCAL       pic 9(04).
           02 DT-MOVIMENTO       pic 9(08).
           02 STATUS-MOV         PIC 9(01).


       WORKING-STORAGE SECTION.

       01 reg-his-E.
           02 Chaves-e.
               03 CODIGO-BEM-E     pic 9(06).
           02 CODIGO-LOCAL-E       pic 9(04).
           02 DT-MOVIMENTO-E       pic 9(08).
           02 STATUS-MOV-E         PIC 9(01).



       01 data-sis.
           02 ano   pic 9999.
           02 mes   pic 99.
           02 dia   pic 99.
       01 desmes.
          02 filler pic x(10) value "Janeiro".
          02 filler pic x(10) value "Fevereiro".
          02 filler pic x(10) value "Marco".
          02 filler pic x(10) value "Abril".
          02 filler pic x(10) value "Maio".
          02 filler pic x(10) value "Junho".
          02 filler pic x(10) value "Julho".
          02 filler pic x(10) value "Agosto".
          02 filler pic x(10) value "Setembro".
          02 filler pic x(10) value "Outubro".
          02 filler pic x(10) value "Novembro".
          02 filler pic x(10) value "Dezembro".
       01 tabela-meses redefines desmes.
           02 mes-t pic x(10) occurs 12 times.
       01 arqst        pic x(2).
       01 op           pic x(1) value spaces.
       01 salva        pic x(1) value spaces.
       01 wigual       pic 9 value zeros.
       01 espaco       pic x(10) value spaces.
       01 op-continua  pic x(1)  value spaces.

       screen section.
       01 tela-inicial.
          02 line 2 col 2 value "Santos,    de            de     .".
          02 line 4 col 30 value "Nome da Empresa".
          02 line 6 col 25 value "Controle de movimentação dos Bens".
          02 line 7 col 25 value "       Menu Principal" highlight.
          02 line 8 col 3 VALUE "1 - Inclusao de Movimento".
          02 line 8 col 40 VALUE "2 - Exclusao de Bem".
          02 line 9 col 40 VALUE "3 - Consulta por codigo".
          02 line 10 col 3 VALUE "4 - Listagem do Movimento".
          02 line 13 col 20 value "5 - Sair ".
          02 Line 18 col 5 value "Digite Sua Escolha".
       01 Tela-inclusao.
          02 line 2 col 2 value "Santos,    de            de     .".
          02 line 4 col 30 value "Nome da Empresa".
          02 line 6 col 29 value "Controle de Patrimonio".
          02 line 7 col 27 value "INCLUSAO DE MOVIMENTO" highlight.
          02 line 9 col 3 VALUE "Codigo do Bem".
          02 line 11 col 3 VALUE "Codigo do Local".
          02 line 9 col 45 VALUE "Data de Movimentação".
          02 line 11 col 45 VALUE "Estatus da Movimentação".
       01 Tela-esclusao.
          02 line 2 col 0 value "Santos,    de            de     .".
          02 line 3 col 27 value "Nome da Empresa".
          02 line 4 col 25 value "Controle de Patrimonio".
          02 line 5 col 20 value "EXCLUSAO DE BEMS" highlight.
          02 line 9 col 3 VALUE "Codigo do BEM".
       01 Tela-consulta.
          02 line 2 col 2 value "Santos,    de            de     .".
          02 line 4 col 30 value "Nome da Empresa".
          02 line 6 col 29 value "Controle de Patrimonio".
          02 line 7 col 27 value "CONSULTA DE MOVIMENTAÇÃO" highlight.
           02 line 9 col 3 VALUE "Codigo do Bem".
          02 line 11 col 3 VALUE "Codigo do Local".
          02 line 9 col 45 VALUE "Data de Movimentação".
          02 line 11 col 45 VALUE "Estatus da Movimentação".

       01 MENSAGENS.
           02 line 21 col 10 value "ERRO: Valor Invalido".
           02 line 21 col 10 value "ERRO: Depto nao encontrado".


       procedure division.
      *-----------------------------------------------------------------
       Inicio.
           Perform abre-arq.
           Perform abertura until op = "5".
           exit program.
      *-----------------------------------------------------------------
       sai.
         exit program.
      *-----------------------------------------------------------------
       abre-arq.

                  OPEN I-O MOVIM.
           IF ARQST NOT = "00"
               DISPLAY "ERRO DE ABERTURA"
               STOP " "
               CLOSE MOVIM
               OPEN OUTPUT MOVIM
               CLOSE MOVIM
               OPEN I-O MOVIM.

      *-----------------------------------------------------------------
       abertura.
           display ESPACO at 0101.
           display tela-inicial at 0101.
           Perform mostra-data.
           accept op at 1845.
           perform trata-opcao.

      *-----------------------------------------------------------------
       trata-opcao.
           move spaces to op-continua
           evaluate op
           when "1"
               perform inclusao until op-continua = "n" or "N"
           when "2"
               perform exclusao until op-continua = "n" or "N"
           when "3"
               perform consulta until op-continua = "n" or "N"
           when "4"
               perform listagem until op-continua = "n" or "N"
           when "5"
               perform sai.
      *-----------------------------------------------------------------
       mostra-data.
           move function current-date to data-sis.
           display dia at 0213.
           display mes-t(mes) at 0219.
           display ano at 0233.
      *-----------------------------------------------------------------
       inclusao.
           perform tela-inclu.
           move zeros to wigual.
           perform inicializar.
           perform testa-codbem until wigual = 1.
           perform recebe-dados.
           perform grava.
           perform continua.
      *-----------------------------------------------------------------
       tela-inclu.
          display erase at 0101.
          display Tela-inclusao at 0101.

          perform mostra-data.
      *-----------------------------------------------------------------
       inicializar.
           move spaces to op op-continua salva.
           move zeros to  CODIGO-BEM-E.
           move zeros to  CODIGO-LOCAL-E.
           move zeros to  DT-MOVIMENTO-E.
           move zeros to  STATUS-MOV-E.
           move zeros to wigual.
           display espaco at 2315.
      *-----------------------------------------------------------------
       recebe-dados.
           perform testa-codbem    until CODIGO-BEM-E not = zeros.
           perform testa-local     until CODIGO-LOCAL-E not = zeros.
           perform testa-mov  until DT-MOVIMENTO-E  not = zeros.
           perform testa-status       until STATUS-MOV-E  not = zeros.
      *-----------------------------------------------------------------
       testa-codbem.
           move 1 to wigual
           move zeros to CODIGO-BEM-E
           accept CODIGO-BEM-E at 0926 with prompt auto
           if CODIGO-BEM-E = spaces or "0000" then
                display espaco at 2321
                display "Digite um codigo diferente de zero." at 2321
                set wigual to 0
           else
                move CODIGO-BEM-E to CODIGO-BEM
                read MOVIM not invalid key perform ja-cadastrado.

       ja-cadastrado.
           display espaco at 2321
           display "Codigo ja  cadastrado" at 2321
           set wigual to 0
           perform testa-codbem.


       testa-local.
           accept CODIGO-LOCAL-E at 1126
           if CODIGO-LOCAL-E = zeros then
                display "Digite o codigo do local." at 2321
                else
                display espaco at 2321.

       testa-mov.
           accept DT-MOVIMENTO-E at 0966
           if DT-MOVIMENTO-E = "00/00/0000" then
                display "Digite a data." at 2321
                else
                display espaco at 2321.

       testa-status.
           accept STATUS-MOV-E at 1166
           if STATUS-MOV-E = zeros then
                display "Digite o Estatus." at 2321
                else
                display espaco at 2321.


       grava.
           display "Salvar <S/N> [ ]" at 2321.
           accept salva at 2335 with prompt auto.
           if salva = "S" or "s" then
                move reg-his-E to reg-his
                write reg-his invalid key perform estuda-erro
                display arqst at 2221.


       continua.
           display espaco at 2321.
           display "Continua <S/N> [ ]" at 2321.
           accept op-continua at 2337 with prompt auto.
           if op-continua = "S" or "s" then
                perform inicializar
                display espaco at 2321.

       exclusao.
           perform inicializar.
           display erase at 0101.
           display Tela-esclusao at 0101.
           perform inicializar.
           perform le-dados.
           if arqst = "00" then
               display "Deseja excluir o registro<S/N> [ ]" at 2319
               accept salva at 2351 with prompt auto
           else
               perform inicializar
               display espaco at 2319
               display "Registro nao encontrado." at 2321.
           if salva = "S" or "s" then
               display espaco at 2319
               Display "Registro apagado." at 2321
               delete MOVIM.
           stop " ".
           display espaco at 2315.
           perform continua.

       estuda-erro.
           display "Codigo nao encontrado." at 2321.
       stop " ".

       consulta.
           display espaco at 0101.
           display Tela-consulta at 0101.
           display "Consulta de Bems" at 0730 with highlight.
           perform le-dados.
           perform continua.


       le-dados.
           perform inicializar.
           perform mostra-data.
           accept CODIGO-BEM-E at 0832.
           move CODIGO-BEM-E to CODIGO-BEM.
           read MOVIM key is CODIGO-BEM invalid key
                display "Registro nao encontrado" at 2320
                move 1 to wigual
                stop " ".
           if arqst = "00" then
              display espaco at 0832
              perform mostra-tela.


       mostra-tela.
       perform inicializar.
           move reg-his to reg-his-E.
           display CODIGO-BEM-E at 0926.
           display CODIGO-LOCAL-E at 1126.
           display DT-MOVIMENTO-E at 0966.
           display STATUS-MOV-E  at 1336.


       altera-dados.
       perform inicializar.
           accept CODIGO-BEM-E at 0832.
           accept CODIGO-LOCAL-E at 1032.
           accept DT-MOVIMENTO-E at 1132.
           accept STATUS-MOV-E  at 1332.


       listagem.
       perform inicializar.
           move reg-his to reg-his-E.
           display CODIGO-BEM-E at 0926.
           display CODIGO-LOCAL-E at 1126.
           display DT-MOVIMENTO-E at 0966.
           display STATUS-MOV-E  at 1336.

       end program MOVIM.
       