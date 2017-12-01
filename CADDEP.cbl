      *TO-DO
      *Consulta não Mostra o departamento, mas mostra o resto
      *na tela de cadastro esta comendo um pedaço do texto, acho que é
      *por causa do tamanho da variavel que tem que ser aceita

       Identification Division.
       Program-Id. CADDPTO.

       Environment Division.
       CONFIGURATION SECTION.
       special-names.
       decimal-point is comma.

       input-output section.
       file-control.
           select DEPTOS assign to disk
           organization indexed
           access mode dynamic
           record key CODIGO-DEPTO
           alternate record key NOME-DEPTO with duplicates
           file status arqst.


       data division.
       file section.
       fd  DEPTOS
       value of file-id is "DEPTOS.DEP".



       01 reg-dptos.
           02 Chaves.
               03 CODIGO-DEPTO   pic x(04).
           02 NOME-DEPTO         pic x(30).
           02 RESPONSAVEL        pic x(30).
           02 DIVISAO            pic x(02).
       01 codidpt  pic x(04).

              WORKING-STORAGE SECTION.
       01 reg-dptos-e.
           02 Chaves-e.
               03 CODIGO-DEPTO-e   pic zzzz(04).
           02 NOME-DEPTO-e         pic x(30).
           02 RESPONSAVEL-e        pic x(30).
           02 DIVISAO-e            pic zz(02).
       01 codidpt-e                pic zzzz(04).
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
       01 espaco       pic x(50) value spaces.
       01 op-continua  pic x(1)  value spaces.

       01 DIVISAO.
           02 filler pic x(10) value "PRESIDENCIA".
           02 filler pic x(10) value "DIRETORIA".
           02 filler pic x(10) value "COMERCIAL".
           02 filler pic x(10) value "OPERACIONAL".
           02 FILLER PIC X(10) VALUE "PRODUCAO".
       01 TAB-DIVISAO redefines DIVISAO.
           02 DIV-T pic x(10) occurs 5 times.
       screen section.
       01 tela-inicial.
       02 BLANK SCREEN.
          02 line 2 col 2 value "Santos,    de            de     .".
          02 line 4 col 30 value "Nome da Empresa".
          02 line 6 col 29 value "Controle de Patrimonio".
          02 line 7 col 27 value "ALTERACAO DE DEPARTAMENTO" highlight.
          02 line 9 col 3 VALUE "1 - Inclusao de novos Deptos".
          02 line 11 col 3 VALUE "2 - Alteracao de Deptos Cadastrados".
          02 line 9 col 45 VALUE "3 - Exclusao de Departamentos".
          02 line 11 col 45 VALUE "4 - Consulta a deptos por nome".
          02 line 15 col 33 value "5 - Sair ".
          02 Line 18 col 5 value "Digite Sua Escolha".
       01 Tela-inclusao.
       02 BLANK SCREEN.
          02 line 2 col 2 value "Santos,    de            de     .".
          02 line 4 col 30 value "Nome da Empresa".
          02 line 6 col 29 value "Controle de Patrimonio".
          02 line 7 col 27 value "INCLUSAO DE NOVOS DEPTOS" highlight.
          02 line 9 col 3 VALUE "Codigo do Departamento".
          02 line 11 col 3 VALUE "Nome do Departamento".
          02 line 9 col 45 VALUE "Nome do Responsavel".
          02 line 11 col 45 VALUE "Numero da Divisao".
       01 Tela-alteracao.
       02 BLANK SCREEN.
          02 line 2 col 2 value "Santos,    de            de     .".
          02 line 4 col 30 value "Nome da Empresa".
          02 line 6 col 29 value "Controle de Patrimonio".
          02 line 7 col 27 value "ALTERACAO DE DEPTOS" highlight.
          02 line 9 col 3 VALUE "Codigo do Departamento".
          02 line 11 col 3 VALUE "Nome do Departamento".
          02 line 9 col 45 VALUE "Nome do Responsavel".
          02 line 11 col 45 VALUE "Numero da Divisao".
       01 Tela-esclusao.
       02 BLANK SCREEN.
          02 line 2 col 0 value "Santos,    de            de     .".
          02 line 3 col 27 value "Nome da Empresa".
          02 line 4 col 25 value "Controle de Patrimonio".
          02 line 5 col 20 value "EXCLUSAO DE DEPARTAMENTOS" highlight.
          02 line 9 col 3 VALUE "Codigo do Departamento".
       01 Tela-consulta.
       02 BLANK SCREEN.
          02 line 2 col 2 value "Santos,    de            de     .".
          02 line 4 col 30 value "Nome da Empresa".
          02 line 6 col 29 value "Controle de Patrimonio".
          02 line 7 col 27 value "CONSULTA A DEPTOS" highlight.
          02 line 9 col 3 VALUE "Codigo do Departamento".
          02 line 11 col 3 VALUE "Nome do Departamento".
          02 line 9 col 45 VALUE "Nome do Responsavel".
          02 line 11 col 45 VALUE "Numero da Divisao".

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

                  OPEN I-O DEPTOS.
           IF ARQST NOT = "00"
               DISPLAY "ERRO DE ABERTURA"
               STOP " "
               CLOSE DEPTOS
               OPEN OUTPUT DEPTOS
               CLOSE DEPTOS
               OPEN I-O DEPTOS.

      *-----------------------------------------------------------------
       abertura.
           display erase at 0101.
           display tela-inicial.
           Perform mostra-data.
           accept op at 1825.
           perform trata-opcao.
      *-----------------------------------------------------------------
       trata-opcao.
           move spaces to op-continua
           evaluate op
           when "1"
               perform inclusao until op-continua = "n" or "N"
           when "2"
               perform alteracao until op-continua = "n" or "N"
           when "3"
               perform exclusao until op-continua = "n" or "N"
           when "4"
               perform consulta until op-continua = "n" or "N"
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
           perform testa-coddpt until wigual = 1.
           perform recebe-dados.
           perform grava.
           perform continua.
      *-----------------------------------------------------------------
       tela-inclu.
          display erase at 0101.
          display Tela-inclusao.
          perform mostra-data.
      *-----------------------------------------------------------------
       inicializar.
       move spaces to op op-continua salva.
           move ZEROS to CODIGO-DEPTO-e.
           move spaces to NOME-DEPTO-e.
           move spaces to RESPONSAVEL-e.
           move zeros to wigual DIVISAO-e.
           display espaco at 2321.
      *-----------------------------------------------------------------
       recebe-dados.
           perform testa-coddpt      until CODIGO-DEPTO-e  not = spaces.
           perform testa-nomedpt     until NOME-DEPTO-e    not = spaces.
           perform testa-responsavel until RESPONSAVEL-e   not = spaces.
           perform testa-divisao     until
           DIVISAO-e not = spaces.

      *-----------------------------------------------------------------
       testa-coddpt.
           move 1 to wigual
           move ZEROS to CODIGO-DEPTO-e.
           accept CODIGO-DEPTO-e at 0926 with prompt auto
           if CODIGO-DEPTO-e = spaces or "0000" then
                display espaco at 2321
                display "Digite um codigo diferente de zero." at 2321
                set wigual to 0
           else
                move CODIGO-DEPTO-e to CODIGO-DEPTO
                read DEPTOS not invalid key perform ja-cadastrado.
      *-----------------------------------------------------------------

       ja-cadastrado.
           display espaco at 2321
           display "Codigo ja cadastrado" at 2321
           set wigual to 0.
           perform testa-coddpt.
      *-----------------------------------------------------------------
       testa-nomedpt.
           accept NOME-DEPTO-e at 1126
           if NOME-DEPTO-e = spaces then
               display espaco at 2321
                display "Digite o nome do departamento." at 2321.


      *-----------------------------------------------------------------
       testa-responsavel.
           accept RESPONSAVEL-e at 0968
           if RESPONSAVEL-e = spaces then
               display espaco at 2321
                display "Digite o nome do responsavel." at 2321.

      *-----------------------------------------------------------------
       testa-divisao.
           accept DIVISAO-e at 1168
           if DIVISAO-e = spaces then
           display espaco at 2321
                display "Digite o Nº da divisao." at 2321.
      *-----------------------------------------------------------------
       grava.
           display espaco at 2321
           display "Salvar <S/N> [ ]" at 2321
           accept salva at 2335 with prompt auto.
           if salva = "S" or "s" then
                move reg-dptos-e to reg-dptos
                write reg-dptos invalid key perform estuda-erro
                display arqst at 2221.
      *-----------------------------------------------------------------
       continua.
           display espaco at 2321
           display "Continua <S/N> [ ]" at 2321.
           accept op-continua at 2337 with prompt auto.
           if op-continua = "S" or "s" then
                perform inicializar
                display espaco at 2321.
      *-----------------------------------------------------------------
       exclusao.
           perform inicializar.
           display erase at 0101.
           display Tela-esclusao.
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
               delete DEPTOS.
           stop " ".
           display espaco at 2315.
           perform continua.
      *-----------------------------------------------------------------
       estuda-erro.
           display "Codigo nao encontrado." at 2321.
       stop " ".
      *-----------------------------------------------------------------
       consulta.
           display erase at 0101.
           display Tela-consulta.
           display "Consulta de Registro" at 0730 with highlight.
           perform le-dados.
           perform continua.
      *-----------------------------------------------------------------
       le-dados.
           perform inicializar.
           perform mostra-data.
           accept CODIGO-DEPTO-e at 0926.
           move CODIGO-DEPTO-e to CODIGO-DEPTO.
           read DEPTOS key is CODIGO-DEPTO invalid key
                display "Registro nao encontrado" at 2321
                move 1 to wigual
                stop " ".
           if arqst = "00" then
              display espaco at 2321
              perform mostra-tela.
      *-----------------------------------------------------------------
       mostra-tela.
       perform inicializar.
           move reg-dptos to reg-dptos-e.
           display "Codigo do Departamento" at 0903.
           display CODIGO-DEPTO-e at 0926.
           display "Nome do Departamento" at 1103.
           display NOME-DEPTO-e at 1126.
           display "Nome do Responsavel" at 0945.
           display RESPONSAVEL-e at 0968.
           display "Numero da Divisao" at 1145.
           display DIVISAO-e at 1168.
      *-----------------------------------------------------------------
       altera-dados.
       perform inicializar.
           accept CODIGO-DEPTO-e at 0926.
           accept NOME-DEPTO-e at 1126.
           accept RESPONSAVEL-e at 0968.
           accept DIVISAO-e at 1168.
      *-----------------------------------------------------------------
       alteracao.
           perform inicializar.
           display erase at 0101.
           display Tela-alteracao.
           perform le-dados.
           if wigual <> 1
              perform altera-dados
              perform recebe-dados
              display "Deseja salvar alteracao <S/N> [ ]" at 2319
              accept salva at 2350 with prompt auto
              if salva = "S" or "s" then
                 move reg-dptos-e to reg-dptos
                 rewrite reg-dptos invalid key perform estuda-erro
                 display espaco at 2315.
           perform continua.
       end program CADDPTO.
