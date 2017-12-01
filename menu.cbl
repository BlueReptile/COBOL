
       PROGRAM-ID menu.

       Environment Division.
       CONFIGURATION SECTION.
       special-names.
       decimal-point is comma.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       data division.
       file section.

       WORKING-STORAGE SECTION.
       01 kk.
         02 op              pic 9(01).
         02 op-continua     pic x(01).
         02 Cont            PIC X(01).
       

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


       screen section.
       01 tela-inicial.
          02 BLANK SCREEN.
          02 line 2 col 2 value "Santos,    de            de     .".
          02 line 4 col 30 value "Nome da Empresa".
          02 line 6 col 25 value "Menu Principal" highlight.
          02 line 8 col 3 VALUE "1 - Cadastro".
          02 line 8 col 40 VALUE "2 - Movimento".
          02 line 9 col 40 VALUE "3 - Sobre".
          02 line 10 col 3 VALUE "4 - Fim".
          02 Line 18 col 5 value "Digite Sua Escolha".

       01 tela-cadastro.
          02 BLANK SCREEN.
          02 line 2 col 2 value "Santos,    de            de     .".
          02 line 4 col 30 value "Nome da Empresa".
          02 line 6 col 25 value "Menu Cadastro" highlight.
          02 line 8 col 3 VALUE "1 - DEPTO".
          02 line 8 col 40 VALUE "2 - LOCAL".
          02 line 9 col 40 VALUE "3 - TIPO".
          02 line 10 col 3 VALUE "4 - RETORNO".
          02 Line 18 col 5 value "Digite Sua Escolha".


       01 tela-movimento.
          02 BLANK SCREEN.
          02 line 2 col 2 value "Santos,    de            de     .".
          02 line 4 col 30 value "Nome da Empresa".
          02 line 6 col 25 value "Menu Movimento" highlight.
          02 line 8 col 3 VALUE "1 - BENS".
          02 line 8 col 40 VALUE "2 - HISTORICO".
          02 line 9 col 40 VALUE "4 - RETORNO".
          02 Line 18 col 5 value "Digite Sua Escolha".

       01 tela-sobre.
          02 BLANK SCREEN.
          02 line 2 col 2 value "Santos,    de            de     .".
          02 line 4 col 30 value "Nome da Empresa".
          02 line 6 col 25 value "Menu Sobre" highlight.
          02 line 8 col 3 VALUE  "Alunos".
          02 line 8 col 25 VALUE "Julio Cezar Franco Almeida - 005048152022".
          02 line 9 col 25 VALUE "Romulo ".
          02 line 10 col 25 VALUE "Carinha".
          02 line 25 col 25 VALUE "Aperte N para sair(   )".



       01 MENSAGEN1.
           02 line 21 col 10 value "ERRO: Valor Invalido".

       PROCEDURE DIVISION.

       abertura.
           display ERASE SCREEN at 0101.
           display tela-inicial at 0101.
           Perform mostra-data.
           accept op at 1845.
           perform trata-opcao.

       Inicio.
           perform abertura.

       retorno.
           perform inicio.

       Cadastro.
           display ERASE SCREEN.
           display tela-cadastro at 0101.
           Perform mostra-data.
           accept op at 1845.
           perform trata-cadastro.

       mostra-data.
           move function current-date to data-sis.
           display dia at 0213.
           display mes-t(mes) at 0219.
           display ano at 0233.

       Movimento.
           display ERASE SCREEN at 0101.
           display tela-movimento at 0101.
           Perform mostra-data.
           accept op at 1845.
           perform trata-cadastro.

       sai.
         stop run.


       Sobre.
           display ERASE SCREEN at 0101.
           Perform mostra-data.
           display tela-sobre at 0101.
           accept op-continua at 2545.


       trata-opcao.
           move spaces to op-continua.
           evaluate op
           when "1"
               perform Cadastro until op-continua = "n" or "N"
           when "2"
               perform Movimento until op-continua = "n" or "N"
           when "3"
               perform Sobre until op-continua = "n" or "N"
           when "4"
               perform sai.


       trata-cadastro.
           move spaces to op-continua.
           evaluate op
           when "1"
               call "CADDEP"
           when "2"
               call "CADLOC"
           when "3"
               call "CADTIPO"
           when "4"
               perform retorno.

       trata-movimento.
           move spaces to op-continua.
           evaluate op
           when "1"
               call "BENS"
           when "2"
               call "MOVIM"
           when "3"
               perform retorno.

       end program menu.
