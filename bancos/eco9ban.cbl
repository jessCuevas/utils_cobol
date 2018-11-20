      *****************************************************************
      ** Nombre      : ECO9BAN                                       **
      ** Descripci¢n : Parametrizacion de bancos para cli/pro        **
      ** Proyecto    : A3ECO                                         **
      *****************************************************************
      ** Programador : JORDIM                                        **
      ** Fecha inicio:                                               **
      ** Fecha fin   :                                               **
      *****************************************************************
      **                       MODIFICACIONES                        **
      *****************************************************************
      **N§ Ver.  Fecha   Program.  Descripci¢n                       **
      *****************************************************************
      **                                                             **
      *****************************************************************

       SPECIAL-NAMES.
           CALL-CONVENTION 66 IS WINAPI
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "STABANEL.SEL".    *> Configuración de banca electrónica
           COPY "FEEEEVAR.SEL".    *> Datos Varios emp. todos los ejer.
           COPY "FEEEEACU.SEL".    *> Para la descrip. de la cuenta
           copy "stabanco.sel".

       DATA DIVISION.
       FILE SECTION.
           COPY "STABANEL.FD".     *> Configuración de banca electrónica
           COPY "FEEEEVAR.FD".     *> Datos Varios emp. todos los ejer.
           COPY "FEEEEACU.FD".     *> Para la descrip. de la cuenta
           copy "stabanco.fd".
       WORKING-STORAGE SECTION.

            COPY "DS-CNTRL.MF".
            COPY "DSSYSINF.CPY".
            COPY "ECO9BAN.CPB".
            COPY "STAWMSG.LKG".
            COPY "A3ECO.EXT".
            COPY "STAERROR.CPY".
            COPY "ECOMTFI.LKG".
            copy "ECOMVAR.CPY".
            copy "A3ASESOR.CPY".
            copy "ECOASOPE.LKG".
            copy "ecociban.lkg".
            COPY "STAPROG.LKG".

       78 Max-Bancos               value 99.

      *--- Para las ayudas -------------------------------------------
       77 Nom-Set-Ayuda            pic x(256).
       77 Nom-Ventana-Ayuda        pic x(32).
       *>------------- VARIABLES PARA EL PASO DE DATOS -----------------
           01 ENTIDAD-DAT          PIC 9(04) COMP-5.
           01 OFICINA-DAT          PIC 9(04) COMP-5.
           01 DC-DAT               PIC 9(02) COMP-5.
           01 NUMCUENTA-DAT        PIC 9(10) COMP-5.
           01 pos                  pic 9(02) comp-5.
       *> - TRATAMIENTO CUENTAS
         77 DIGITOS-MASCARA       PIC 99.
         77 DIGITOS-CUENTA-FTCC   PIC X(4) COMP-5.
         77 CUENTA-FTCC           PIC X(12).

       *> - VARIABLES GENERALES ----------------------------------------
         77 CONTADOR            PIC 9(10). *> Contador para procesos
         77 COD-PROX-BAN        PIC 9(10). *> Próximo código de banco
         77 SALIR               PIC X(01). *> Booleano
         77 COD-ACT-VAR-TEMPOR  PIC X(10). *> Cópia de seg.
         77 ALGUNA-MARCA        PIC X(01). *> Booleano para la marca
         77 ERA-PREDET          PIC X(01). *> Predeterminado
         77 EXISTE-CODIGO       PIC X(01). *> Booleano existe codigo
         77 COD-BAN-BAE-TMP     PIC 9(04). *> No comprimido para cadenas

         77 CUENTA-REPETIDA     PIC X(01). *> Cuenta repetida
         77 CCC-REPETIDA        PIC X(01). *> C.C.C. repetida
         77 COD-BAN-LISTA       PIC 9(10).

       *>-  VARIABLES DEL MODULO STAFILE ------------------------------
         COPY "STAFILE.LKG".

       *>-  VARIABLES DEL MODULO ECOMCTA ------------------------------
         77 CUENTA            PIC 9(12).   *> Temporal para la cuenta.
         77 TIPO-BUSQUEDA     PIC X.
         77 MODULO            PIC 9 VALUE 2.
         77 NRO-ORDEN         PIC 9(9) COMP-5.
         77 TIPO-LINEA        PIC 99 COMP-5.
         77 CLI-PRO           PIC X.
         77 COM-VEN           PIC X.
         77 SALDO-ACUMULADO   PIC S9(12)V99 COMP-5.
         77 MES-APUNTE        PIC 99 VALUE 0.
       *> --------------------------------------------------------------

       *> - MODULO ECOMNCT --------------------------------------------
         77 NP-COWM-NCT       PIC 99 COMP-5 VALUE 14.
         77 NIVEL-ALTA        PIC XX.
       *> --------------------------------------------------------------

       *> MODULO STABANCO ----------------------------------------------
           77 NP-STABANCO                 PIC 9(01) COMP-5.
           77 CODIGO-BAN                  PIC 9(04).
           77 NOMBRE-BAN                  PIC X(25).
           77 TEC-PUL                     PIC X(02).
           77 FUNCION-BAN                 PIC X(01).
       *> --------------------------------------------------------------

       *> MODULO STAPROV -----------------------------------------------
           77 NP-STAPROV                  PIC 9 COMP-5.
              01 CLAVE-F4.
                 03 CODIGO-F4                     PIC X(4).
                 03 DESC-F4                       PIC X(39).
                 03 F4-TROBAT                     PIC X.
                    88 F4-OK VALUE "S" FALSE "N".
       *> --------------------------------------------------------------

            01 FIN-PROGRAMA               PIC X.
            01 TECLA                      PIC X.

            01 II                         PIC 99.
            01 JJ                         PIC 9999.

            01 FCD-ERROR.
               03 ERROR-1                 PIC X.
               03 ERROR-2                 PIC X.
            01 CLAVE-VAR                  PIC 9.
       *>----------------------- VAR: ERRORES --------------------------

           01 NP-STASOMBR              PIC X.
           01 IND-INIT                 PIC X.
           01 CTRL-SOMBRA              PIC X.

       *>----------- VARIABLES DEL MODUL MASCARA_IMPORTE ---------------
           01 IMP-ENTRADA              PIC S9(12)V99.
           01 LON-CAMPO                PIC 99.
           01 MONEDA                   PIC X.
           01 IMP-SALIDA               PIC X(17).
       *>----------- VARIABLES DEL MODUL REDONDEO ----------------------
           01 IMPORTE-ENTRADA          PIC S9(12)V999.
           01 IMPORTE-SALIDA           PIC S9(12)V99.

       *>---------------------------------------------------------------

       *> MODULO STADIGC (COMPROBACIÓN DE DÍGITIOS DE CONTROL) --------
           77 NP-STADIGC                  PIC 9 COMP-5.
           77 CODIGO-CUENTA               PIC X(20).
           77 RESPUESTA                   PIC X.
           77 COD-CORRECTO                PIC 9(2).
       *> -------------------------------------------------------------

       *>----------- VARIABLES DEL MODULO ECOMMAIL --------------------
           copy "ECOMAIL.cpy".
       *> -------------------------------------------------------------

       *>----------- VARIABLES DEL MÓDULO STADIREC ---------------------
           77 NP-Stadirec        PIC 9(01) COMP-5.
           77 Camino-Acceso-Dir  PIC X(40).
           77 Tecla-Pulsada      PIC X(02).

       01 StsFic pic x.
       01 Texto-err-Banco  pic x(100).
       01 VerCCC           pic x.

       *>>
       78 long-var value length reg-feeeevar.
       77 reg-feeeevar-aux  pic x(long-var).
       77 cta-con-var-tmp   pic 9(12) comp-x.

       01 ccc-ban-var-tmp.
          03 cc1-ban-var-tmp pic 9(04) comp-5.
          03 cc2-ban-var-tmp pic 9(04) comp-5.
          03 cc3-ban-var-tmp pic 9(04) comp-5.
          03 cc4-ban-var-tmp pic 9(10) comp-5.
       *>>

       77 cuenta-repetida-2 pic x(01).
       77 ccc-repetida-2 pic x(01).
       77 contador-2     PIC 9(10).

       01 CCC-BAN-VAR-tmp2.
         07 CC1-BAN-VAR-tmp2    PIC 9(4).
         07 CC2-BAN-VAR-tmp2    PIC 9(4).
         07 CC3-BAN-VAR-tmp2    PIC 9(2).
         07 CC4-BAN-VAR-tmp2    PIC 9(10).


       LINKAGE SECTION.
           01 Funcion           pic XX.
           copy "Eco9ban.lkg".
           01 SALIDA            PIC XX.
           01 Op-Exito          pic X.

       PROCEDURE DIVISION USING FUNCION,
                                CTA-CON-VAR-LKG,
                                COD-ACT-VAR-LKG,
                                Tabla-Bancos-Lkg
                                Numero-Bancos-Lkg
                                SALIDA,
                                OP-EXITO.
       DECLARATIVES.
           COPY "STABANEL.ERF".    *> Configuración de banca electrónica
           COPY "FEEEEVAR.ERF".    *> Datos Varios emp. todos los ejer.
       END DECLARATIVES.

       *>---------------------------------------------------------------

       INICIO.
            PERFORM OPERACIONES-INICIALES
            PERFORM TRATAMIENTO UNTIL FIN-PROGRAMA = "S"
            PERFORM OPERACIONES-FINALES
       .

      *****************************************************************
       TRATAMIENTO.
          PERFORM CUENTA-NUM-A-CUENTA-ALF
          PERFORM PRESENTAR-PANEL
          PERFORM CUENTA-ALF-A-CUENTA-NUM
          EVALUATE ECO9BAN-SALIDA
             WHEN "PV" *> Posicionar ventana
                CALL "STAPOSIC" USING ECO9BAN-HANDLE-Ventana
                CANCEL "STAPOSIC"
             WHEN "PM" *> Poner máscaras
                PERFORM PONER-MASCARAS
             WHEN "CT" *> Cargar Tabla
                PERFORM Cargar-Tabla
             WHEN "VC" *> Verificar Campos
                PERFORM VERIFICAR-CAMPOS
             WHEN "AR" *> Alta registro
                PERFORM ALTA-REGISTRO
             WHEN "BR" *> Baja
                PERFORM BAJA-REGISTRO
             WHEN "MR" *> Modificar registro
                PERFORM MODI-REGISTRO
             WHEN "CE" *> Cargar Entry-fields
                PERFORM Cargar-EntryFields
             WHEN "FB" *> F4 - Bancos
                PERFORM F4-Bancos
             when "fb" *> F4 - Bancos
                perform F4-Bco-A3ECO
             WHEN "fB" *> TAB - Bancos
                PERFORM TAB-Bancos
             WHEN "FC" *> F4 - Cuentas
                PERFORM F4-CUENTAS
             WHEN "fC" *> TAB - Cuentas
                PERFORM TAB-CUENTAS
             WHEN "FM" *> F4 - Mail
                PERFORM F4-MAIL
             WHEN "FP" *> F4 - Provincia
                PERFORM F4-PROVINCIA
             WHEN "fP" *> TAB - Provincia
                PERFORM TAB-PROVINCIA
             WHEN "FS" *> F4 - CSB
                PERFORM F4-CSB
             WHEN "FL" *> F4 - Local
                PERFORM F4-LOCAL
             WHEN "MX" *> Mensaje Max 100 bancos
                PERFORM MENSAJE-MAX-100
             WHEN "OC" *> Obtener Camino CSB
             *>vcc PERFORM OBTENER-CSB
                   PERFORM CARGAR-BANCA *>vcc actuo como "CB"
             *> GRABACIÓN POR PARTES
             WHEN "ME" *> Modificar Banca Electrónica
                PERFORM MODIFICAR-ELE
             WHEN "MB" *> Modificar Datos Bancarios
                PERFORM MODIFICAR-BAN
             WHEN "AB" *> Alta Datos Bancarios
                PERFORM ALTA-BAN
             WHEN "CB"
                PERFORM CARGAR-BANCA
      *      WHEN "CR" *> Aceptamos elemento de la lista para llamada
                       *> con función
      *         PERFORM SALIR-ACEPTANDO

             WHEN "AC"
                PERFORM ACTIVAR-COMPTES-REDUITS
             WHEN "EC"
                *> Refrescar/Expandir compte.
                PERFORM EXPANDIR-CUENTA

             WHEN "ES" *> Sortir del programa
                MOVE "S"  TO FIN-PROGRAMA
                move "ES" to Salida

             WHEN "CR" *> PASAR DATOS BANCOS A IMPRESOS
                PERFORM PASAR-DATOS
             WHEN "S"
                PERFORM POP-SCREENSET
             WHEN "IB"
                perform Calcular-IBan

          END-EVALUATE
       .
      *****************************************************************

       OPERACIONES-INICIALES.
          MOVE "N" TO FIN-PROGRAMA
          PERFORM INICIALIZAR-SCREENSET
          PERFORM INICIALIZAR-VARIABLES
          PERFORM Abrir-Ficheros
          PERFORM EVALUAR-FUNCION
       .

       INICIALIZAR-VARIABLES.
           INITIALIZE CLAVE-VAR
           INITIALIZE VerCCC
           MOVE "Bancos de la empresa" TO ECO9BAN-TITULO
           move "S" to Op-Exito
       .

       ABRIR-FICHEROS.
         initialize tabla-operaciones
         move "A" to operacion(tfi-stabanco)
         MOVE "A" TO OPERACION(tfi-feeeeVAR)
         MOVE "A" TO OPERACION(TFI-STABANEL)
         MOVE "A" TO OPERACION(TFI-FEEEEACU) *> Utilizado por ECOMCTA
         MOVE "A" TO OPERACION(TFI-FEEEEADA) *> Utilizado por ECOMCTA
         *> BORRAR -

         perform Modulo-TFI
       .

       OPERACIONES-FINALES.
          PERFORM POP-SCREENSET
          PERFORM CERRAR-FICHEROS
          EXIT PROGRAM
       .

       .

       CERRAR-FICHEROS.
          PERFORM Modulo-TFI
       .


      *****************************************************************
       PRESENTAR-PANEL.
          initialize Nom-Set-Ayuda
          move "DBOX-BANCOS" to Nom-Ventana-Ayuda

          Call var-presentar-panel-ayuda-tot using DS-CONTROL-BLOCK
                                                   DS-EVENT-BLOCK
                                                   ECO9BAN-DATA-BLOCK
                                                   ECO9BAN-CALC
                                                   ECO9BAN-HELP
                                                   Nom-Set-Ayuda
                                                   Nom-Ventana-Ayuda
          PERFORM MIRARERRORDSGRUN
       .

       COPY "MIRAERR.CBL" REPLACING
           ==screenset-data-block== BY ==ECO9BAN-DATA-BLOCK==.

       INICIALIZAR-SCREENSET.
           INITIALIZE ECO9BAN-DATA-BLOCK, DS-CONTROL-BLOCK
           MOVE ECO9BAN-DATA-BLOCK-VERSION-NO TO
                DS-DATA-BLOCK-VERSION-NO
           MOVE ECO9BAN-VERSION-NO TO DS-VERSION-NO
           MOVE "ECO9BAN" TO DS-SET-NAME
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE DS-SCREEN-NOCLEAR TO DS-CONTROL-PARAM
       .

       POP-SCREENSET.
          IF DS-CONTROL <> "S"
             MOVE DS-QUIT-SET TO DS-CONTROL
             PERFORM PRESENTAR-PANEL
          END-IF
       .

      ******************************************************************
       MASCARA-IMPORTE.
           MOVE 13 TO LON-CAMPO
           CALL "MASCARA_IMPORTE" USING IMP-ENTRADA
                                        LON-CAMPO
                                        ECO9BAN-MONEDA
                                        IMP-SALIDA
       .

       INICIALIZAR-PANEL.
      *    MOVE ECO-FECHA-DATOS TO FECHA-DATOS-AUX

      *    MOVE ECO-CODIGO-EMPRESA TO CODI5
      *    MOVE CODI5 TO ECO9BAN-COD-EMPRESA
      *    MOVE ECO-NOMBRE-EMPRESA TO ECO9BAN-NOM-EMPRESA
      *    CALL "HALLAR_LITERAL_EXT" USING ECO-FECHA-INI-EJER,
      *                                    ECO9BAN-EJERCICIO

      *    MOVE ECO-MONEDA-EMPRESA TO ECO9BAN-MONEDA
      *    move eco-tipo-empresa to ECO9BAN-TIPO-EMPRESA
       .


       PRESENTA-ERROR.
           IF ERROR-1 <> "0"
               MOVE "S" TO FIN-PROGRAMA
               PERFORM ERRORES
               PERFORM CERRAR-FICHEROS
               PERFORM OPERACIONES-FINALES
           END-IF
       .

       ERRORES.
       *> PROCEDIMIENTO QUE GENERA MENSAJES DE ERROR.
           MOVE 7 TO NP-STAWERR
           MOVE 4 TO NP-STASOMBR
           MOVE "S" TO IND-INIT
           MOVE "W" TO CTRL-SOMBRA
           *> LLAMAMOS AL MODULO
           CALL "STAWERR" USING NP-STAWERR,
                                NP-STASOMBR,
                                FCD-ERROR,
                                IND-INIT,
                                CTRL-SOMBRA,
                                CAMINO-ERRONEO,
                                FICHERO-ERRONEO
           CANCEL "STAWERR"
       .

       *> -------------------------------------------------------------

       Mover-Datos-Fichero-Lista.
         MOVE CONTADOR TO ECO9BAN-COD-BAN-LISTA(CONTADOR)
                          Cod-Banco-Lkg(Contador)

         MOVE CC1-BAN-VAR TO CODIGO-BAN
         PERFORM CARGAR-DESCRIPCION
         MOVE NOMBRE-BAN
              TO ECO9BAN-DES-CC1-BAN-VAR-LISTA(CONTADOR)
                 Nombre-Entidad-Lkg(Contador)

         *> Datos de la oficina
         MOVE OFI-BAN-VAR
              TO ECO9BAN-OFI-BAN-VAR-LISTA(CONTADOR)
                 Nombre-Oficina-Lkg(Contador)
         MOVE Dom-BAN-VAR
              TO Domicilio-Oficina-Lkg(Contador)

         *> Datos del CCC.
         MOVE CC1-BAN-VAR
              TO CC1-Ban-Var-Lkg(Contador)
         MOVE CC2-BAN-VAR
              TO CC2-Ban-Var-Lkg(Contador)
         MOVE CC3-BAN-VAR
              TO CC3-Ban-Var-Lkg(Contador)
         MOVE CC4-BAN-VAR
              TO CC4-Ban-Var-Lkg(Contador)

         initialize EstCIBan
         MOVE  CC1-BAN-VAR   to CIBan-CCC-Bco
         MOVE  CC2-BAN-VAR   to CIBan-CCC-ofi
         MOVE  CC3-BAN-VAR   to CIBan-CCC-Dc
         MOVE  CC4-BAN-VAR   to CIBan-CCC-Cta
         call var-CCCtoIBAN using EstCIban
         move CIban-Cod to ECO9BAN-IBAN-VAR-LISTA(CONTADOR)

       IF CTA-CON-VAR = 0 THEN
            MOVE SPACES TO ECO9BAN-CTA-CON-VAR-LISTA(CONTADOR)
         ELSE
            MOVE CTA-CON-VAR TO
                 ECO9BAN-CTA-CON-VAR-LISTA(CONTADOR)(1:ECO-DIGITOS)
         END-IF
         move Cta-Con-Var to Cta-Banco-Lkg(Contador)

         IF OMI-BAN-VAR = "S" THEN
            MOVE "þ" TO ECO9BAN-OMI-BAN-VAR-LISTA(CONTADOR)
         ELSE
            MOVE " " TO ECO9BAN-OMI-BAN-VAR-LISTA(CONTADOR)
         END-IF
         MOVE "-" TO ECO9BAN-GUION(CONTADOR)
       .

       *> Le damos al tabulador de los bancos
       CARGAR-DESCRIPCION.
           MOVE "B" TO FUNCION-BAN
           PERFORM MODULO-STABANCO
           IF TEC-PUL<>"CR" THEN
              MOVE "Inexistente" TO NOMBRE-BAN
           END-IF
       .

       *> Módulo standar de acceso a bancos
       MODULO-STABANCO.
           CALL "STABANCO" USING NP-STABANCO,
                                 FUNCION-BAN,
                                 ECO-CAMINO-ACCESO-ENTORNO,
                                 CODIGO-BAN,
                                 NOMBRE-BAN,
                                 TEC-PUL,
                                 OP-EXITO
           CANCEL "STABANCO"
       .

       *> Proceso que carga la tabla en memoria
       Cargar-Tabla.
         *> Inicializo la tabla de linkage
         initialize Tabla-Bancos-Lkg
                    Numero-Bancos-Lkg

         INITIALIZE REG-FEEEEVAR
         MOVE "BAN"  TO COD-REG-VAR
         MOVE SPACES TO COD-ACT-VAR
         MOVE    00  TO CTA-CON-VAR
         MOVE 1 TO CLAVE-VAR
         CALL "ST-VAR" USING REG-FEEEEVAR, ">=", CLAVE-VAR, FCD-ERROR

         IF ERROR-1 <> "0" AND FCD-ERROR <> "23" THEN
            MOVE "N"  TO OP-EXITO
            PERFORM PRESENTA-ERROR
         END-IF


         INITIALIZE CONTADOR
                    contador-2
         MOVE 0 TO CONTADOR

         perform inicializarprogreso
         PERFORM UNTIL (ERROR-1 <> "0" AND FCD-ERROR <> "9D") OR
                       CONTADOR>=Max-Bancos OR
                       COD-REG-VAR <> "BAN"
           CALL "RN-VAR" USING "N", FCD-ERROR
           IF (ERROR-1 = "0" OR FCD-ERROR = "9D") AND
              COD-REG-VAR = "BAN" THEN
              perform comprobar-cuenta-repetida-2
              ADD 1 TO CONTADOR
              PERFORM MOVER-DATOS-FICHERO-LISTA
              PERFORM CARGAR-DESCRIPCION
           END-IF
         END-PERFORM

         perform finalizarprogreso

         MOVE CONTADOR TO ECO9BAN-ITEMS-LISTA
         IF ECO9BAN-ITEM-LISTA = 0 THEN
            MOVE 1 TO ECO9BAN-ITEM-LISTA
         END-IF

         *> Número de bancos de las lista
         move Eco9Ban-Items-Lista to Numero-Bancos-Lkg

         *> Controlamos que nunca se salga de madre
         IF ECO9BAN-ITEM-LISTA> ECO9BAN-ITEMS-LISTA THEN
            MOVE ECO9BAN-ITEMS-LISTA TO ECO9BAN-ITEM-LISTA
         END-IF
         if a3Asesor-Integrado = "S"
            move VerCCC to eco9ban-Integrado
         end-if

       .

       *> -------------------------------------------------------------

       *> Se encarga de verificar si los campos están bien informados
       *> antes de proceder a la alta.
       VERIFICAR-CAMPOS.
         MOVE "OK" TO ECO9BAN-SALIDA
         IF ECO9BAN-CTA-CON-VAR = 0 THEN
            MOVE "NO" TO ECO9BAN-SALIDA
            MOVE 4    TO ECO9BAN-CAMPO-ERROR
         END-IF
      *  IF ECO9BAN-CC2-BAN-VAR = 0 THEN     *> set-focus
      *     MOVE "NO" TO ECO9BAN-SALIDA
      *     MOVE 2    TO ECO9BAN-CAMPO-ERROR
      *  END-IF
      *  IF ECO9BAN-CC4-BAN-VAR = 0 THEN
      *     MOVE "NO" TO ECO9BAN-SALIDA
      *     MOVE 1    TO ECO9BAN-CAMPO-ERROR
      *  END-IF
         IF ECO9BAN-CC1-BAN-VAR = 0 THEN
            MOVE "NO" TO ECO9BAN-SALIDA
            MOVE 3    TO ECO9BAN-CAMPO-ERROR *> Código del campo que ha
         END-IF                              *> dado error para hacer el


         IF ECO9BAN-SALIDA = "NO" THEN
            PERFORM MENSAJE-NO-CEROS
         END-IF

         *> Comprobamos los dígitos de control
         IF ECO9BAN-SALIDA <> "NO" THEN
            PERFORM MODULO-STADIGC
         END-IF
         IF ECO9BAN-SALIDA <> "NO" THEN
            PERFORM COMPROBAR-CUENTA-REPETIDA
         END-IF
       .

       MODULO-STADIGC.
         INITIALIZE CODIGO-CUENTA
         STRING ECO9BAN-CC1-BAN-VAR DELIMITED BY SIZE
                ECO9BAN-CC2-BAN-VAR DELIMITED BY SIZE
                ECO9BAN-CC3-BAN-VAR DELIMITED BY SIZE
                ECO9BAN-CC4-BAN-VAR DELIMITED BY SIZE
                INTO CODIGO-CUENTA

         MOVE 4 TO NP-STADIGC
         INITIALIZE RESPUESTA, COD-CORRECTO
         CALL "STADIGC" USING NP-STADIGC
                              CODIGO-CUENTA
                              RESPUESTA
                              COD-CORRECTO
         CANCEL "STADIGC"

         IF RESPUESTA="N"
            initialize Texto-err-Banco
            if eco9ban-integrado = "S"
               MOVE stawmsg-ok    TO STAWMSG-BOTON
               move "Si desea corregirlo debe acceder a A3Asesor" to
                   Texto-err-Banco
            else
               MOVE STAWMSG-YESNO    TO STAWMSG-BOTON
               move
               "¿Desea que la aplicación lo corrija automáticamente?"
                   to Texto-err-Banco
            end-if
            INITIALIZE STAWMSG-TEXTO
            STRING "Los dígitos de control del número de cuenta "
                   DELIMITED BY SIZE
                   "no son correctos."        DELIMITED BY SIZE
                   X"0A"                      DELIMITED BY SIZE
                   "El C.C.C. debería ser el siguiente:"
                   DELIMITED BY SIZE
                   X"0A"X"0A"                 DELIMITED BY SIZE
                   ECO9BAN-CC1-BAN-VAR        DELIMITED BY SIZE
                   "-"                        DELIMITED BY SIZE
                   ECO9BAN-CC2-BAN-VAR        DELIMITED BY SIZE
                   "-"                        DELIMITED BY SIZE
                   COD-CORRECTO               DELIMITED BY SIZE
                   "-"                        DELIMITED BY SIZE
                   ECO9BAN-CC4-BAN-VAR        DELIMITED BY SIZE
                   X"0A"X"0A"                 DELIMITED BY SIZE
      *       "¿Desea que la aplicación lo corrija automáticamente?"
                   Texto-err-Banco
                   DELIMITED BY SIZE
                   INTO STAWMSG-TEXTO
            *> VICTOR - Revisar si es cliente / proveedor
            MOVE "Código Cuenta Cliente Incorrecto" TO
                 STAWMSG-TITULO

            MOVE STAWMSG-WARNING  TO STAWMSG-ICONO

            PERFORM Mostrar-Mensaje

            IF STAWMSG-RETORNO=STAWMSG-RETURN-YES
               MOVE COD-CORRECTO TO ECO9BAN-CC3-BAN-VAR
            END-IF
         END-IF
       .

       *>---------------------------------------------------------------

       *> Traspasa los datos
       DATOS-FICHERO-TO-PANTALLA.
         MOVE  CTA-CON-VAR      TO  ECO9BAN-CTA-CON-VAR

         MOVE  CC1-BAN-VAR      TO  ECO9BAN-CC1-BAN-VAR
         MOVE  CC2-BAN-VAR      TO  ECO9BAN-CC2-BAN-VAR
         MOVE  CC3-BAN-VAR      TO  ECO9BAN-CC3-BAN-VAR
         MOVE  CC4-BAN-VAR      TO  ECO9BAN-CC4-BAN-VAR
         MOVE  OFI-BAN-VAR      TO  ECO9BAN-OFI-BAN-VAR
         MOVE  DOM-BAN-VAR      TO  ECO9BAN-DOM-BAN-VAR
         MOVE  MUN-BAN-VAR      TO  ECO9BAN-MUN-BAN-VAR
         MOVE  CPO-BAN-VAR      TO  ECO9BAN-CPO-BAN-VAR
         MOVE  TLF-BAN-VAR      TO  ECO9BAN-TLF-BAN-VAR
         MOVE  FAX-BAN-VAR      TO  ECO9BAN-FAX-BAN-VAR
         MOVE  PER-BAN-VAR      TO  ECO9BAN-PER-BAN-VAR
         MOVE  MAIL-BAN-VAR     TO  ECO9BAN-MAIL-BAN-VAR
         IF OMI-BAN-VAR = "S" THEN
            MOVE 1 TO ECO9BAN-OMI-BAN-VAR
         ELSE
            MOVE 0 TO ECO9BAN-OMI-BAN-VAR
         END-IF
         *> Descripcion de banco
         PERFORM DESCRIPCION-BANCO
         *> Descripcion de cuenta
         PERFORM DESCRIPCION-CUENTA
         *> Descripcion de la provincia
         PERFORM DESCRIPCION-PROVINCIA
         *> Datos de la banca electrónica
         PERFORM CARGAR-BANCA
         *>Calculo del IBAN
         PERFORM calcular-iban
       .


       *> Traspasa los datos
       DATOS-PANTALLA-TO-FICHERO.
         MOVE ECO9BAN-CTA-CON-VAR       TO CTA-CON-VAR

         MOVE ECO9BAN-CC1-BAN-VAR       TO CC1-BAN-VAR
         MOVE ECO9BAN-CC2-BAN-VAR       TO CC2-BAN-VAR
         MOVE ECO9BAN-CC3-BAN-VAR       TO CC3-BAN-VAR
         MOVE ECO9BAN-CC4-BAN-VAR       TO CC4-BAN-VAR
         IF ECO9BAN-OMI-BAN-VAR = 1 THEN
            MOVE "S" TO OMI-BAN-VAR
         ELSE
            MOVE "N" TO OMI-BAN-VAR
         END-IF
         MOVE ECO9BAN-OFI-BAN-VAR       TO OFI-BAN-VAR
         MOVE ECO9BAN-DOM-BAN-VAR       TO DOM-BAN-VAR
         MOVE ECO9BAN-MUN-BAN-VAR       TO MUN-BAN-VAR
         MOVE ECO9BAN-CPO-BAN-VAR       TO CPO-BAN-VAR
         MOVE ECO9BAN-TLF-BAN-VAR       TO TLF-BAN-VAR
         MOVE ECO9BAN-FAX-BAN-VAR       TO FAX-BAN-VAR
         MOVE ECO9BAN-PER-BAN-VAR       TO PER-BAN-VAR
         MOVE ECO9BAN-MAIL-BAN-VAR      TO MAIL-BAN-VAR
         PERFORM GRABAR-BANCA
       .


       *> Da de alta el registro
       ALTA-REGISTRO.
         PERFORM BUSCAR-BANCO-LIBRE

         INITIALIZE REG-FEEEEVAR
         MOVE "BAN" TO COD-REG-VAR
         MOVE COD-PROX-BAN    TO COD-ACT-VAR

         PERFORM DATOS-PANTALLA-TO-FICHERO
         CALL "WR-VAR" USING FCD-ERROR
         IF ERROR-1 <> "0" AND FCD-ERROR <> "9D" THEN
            MOVE "N"  TO OP-EXITO
            PERFORM PRESENTA-ERROR
         END-IF
         IF OMI-BAN-VAR = "S" THEN
            PERFORM REORGANIZAR-MARCAS
         END-IF
      *  PERFORM PONER-UNA-MARCA
       .

       *> Busca el primer hueco correlativo en COD-BAN-BCP
       BUSCAR-BANCO-LIBRE.
         INITIALIZE CONTADOR
         MOVE 0   TO CONTADOR
         MOVE "N" TO SALIR
         PERFORM UNTIL CONTADOR >= Max-Bancos OR SALIR = "S"
           INITIALIZE REG-FEEEEVAR
           ADD 1 TO CONTADOR
           MOVE "BAN"    TO COD-REG-VAR
           MOVE CONTADOR TO COD-ACT-VAR
           MOVE 1 TO CLAVE-VAR
           CALL "ST-VAR" USING REG-FEEEEVAR, ">=", CLAVE-VAR, FCD-ERROR
           IF ERROR-1 <> "0" AND FCD-ERROR <> "23" THEN
             MOVE "N"  TO OP-EXITO
             PERFORM PRESENTA-ERROR
           END-IF
           CALL "RN-VAR" USING "N",
                               FCD-ERROR
           IF ERROR-1 <> "0" AND FCD-ERROR <> "9D"
              OR COD-REG-VAR <> "BAN" THEN
              MOVE CONTADOR TO COD-PROX-BAN
              MOVE "S"      TO SALIR
           END-IF
         END-PERFORM
       .

       *> -------------------------------------------------------------
       *> Borra el registro actual.
       BAJA-REGISTRO.
         IF ECO9BAN-ITEM-LISTA > 0 THEN
           INITIALIZE REG-FEEEEVAR
           MOVE "BAN"              TO COD-REG-VAR
           MOVE ECO9BAN-COD-BAN-LISTA(ECO9BAN-ITEM-LISTA) TO
                COD-ACT-VAR
           MOVE 1 TO CLAVE-VAR
           CALL "ST-VAR" USING REG-FEEEEVAR, ">=", CLAVE-VAR, FCD-ERROR
           IF ERROR-1 = "0" THEN
              CALL "RN-VAR" USING "N", FCD-ERROR
              IF ERROR-1 = "0" OR FCD-ERROR = "9D" AND
                 ECO9BAN-COD-BAN-LISTA(ECO9BAN-ITEM-LISTA)=COD-ACT-VAR
                 AND COD-REG-VAR = "BAN" THEN

                 PERFORM MENSAJE-CONFIRMAR-BAJA
                 IF STAWMSG-RETORNO = STAWMSG-RETURN-YES THEN
                    CALL "DE-VAR" USING FCD-ERROR
                    IF ERROR-1 <> "0" AND FCD-ERROR <> "9D" THEN
                       MOVE "N"  TO OP-EXITO
                       PERFORM PRESENTA-ERROR
                    END-IF
                    IF OMI-BAN-VAR = "S" AND ECO9BAN-ITEMS-LISTA>1 THEN
                       PERFORM MENSAJE-PREDETERMINADO-BAJA
                       PERFORM MARCAR-PRIMERO
                    END-IF
                 END-IF
              END-IF
           ELSE
             IF FCD-ERROR <> "23" THEN
                MOVE "N"  TO OP-EXITO
                PERFORM PRESENTA-ERROR
             END-IF
           END-IF
           PERFORM REORGANIZAR-LISTA
         END-IF
       .

       *> PROCEDIMIENTOS DE MARCAS OMISIÓN ----------------------------

       *> Marca a "N" el IND-OMI-BCP de todos los registros menos el
       *> acutal
       REORGANIZAR-MARCAS.
         INITIALIZE COD-ACT-VAR-TEMPOR
         MOVE COD-ACT-VAR TO COD-ACT-VAR-TEMPOR

         INITIALIZE REG-FEEEEVAR
         MOVE "BAN"           TO COD-REG-VAR
         MOVE 1               TO CLAVE-VAR
         MOVE SPACES          TO COD-ACT-VAR
         CALL "ST-VAR" USING REG-FEEEEVAR, ">=", CLAVE-VAR, FCD-ERROR

         IF ERROR-1 = "0" OR FCD-ERROR = "9D" THEN
            INITIALIZE CONTADOR
            MOVE 0 TO CONTADOR

            PERFORM UNTIL (ERROR-1 <> "0" AND FCD-ERROR <> "9D") OR
                          CONTADOR>=Max-Bancos OR
                          COD-REG-VAR <> "BAN"
              CALL "RN-VAR" USING "N", FCD-ERROR
              IF (ERROR-1 = "0" OR FCD-ERROR = "9D") AND
                 COD-REG-VAR = "BAN" THEN
                 ADD 1 TO CONTADOR
                 IF COD-ACT-VAR <> COD-ACT-VAR-TEMPOR THEN
                    MOVE "N" TO OMI-BAN-VAR
                    CALL "RW-VAR" USING FCD-ERROR
                    IF ERROR-1 <> "0" AND FCD-ERROR <> "9D" THEN
                       MOVE "N" TO OP-EXITO
                       PERFORM PRESENTA-ERROR
                    END-IF
                 END-IF
              END-IF
            END-PERFORM
         ELSE
           IF FCD-ERROR <> "23" THEN
              MOVE "N" TO OP-EXITO
              PERFORM PRESENTA-ERROR
           END-IF
         END-IF
       .

       *> Pone la marca en el primero
       MARCAR-PRIMERO.
         INITIALIZE REG-FEEEEVAR
         MOVE "BAN"  TO COD-REG-VAR
         MOVE 1      TO CLAVE-VAR
         MOVE SPACES TO COD-ACT-VAR
         CALL "ST-VAR" USING REG-FEEEEVAR, ">=", CLAVE-VAR, FCD-ERROR
         IF ERROR-1 = "0" OR FCD-ERROR = "9D" THEN
            CALL "RN-VAR" USING "N", FCD-ERROR
            IF ERROR-1 = "0" AND COD-REG-VAR = "BAN" THEN
               MOVE "S" TO OMI-BAN-VAR
               CALL "RW-VAR" USING FCD-ERROR
               IF ERROR-1 <> "0" AND FCD-ERROR <> "9D" THEN
                  MOVE "N" TO OP-EXITO
                  PERFORM PRESENTA-ERROR
               END-IF
            END-IF
         ELSE
           IF FCD-ERROR <> "23" THEN
              MOVE "N" TO OP-EXITO
              PERFORM  PRESENTA-ERROR
           END-IF
         END-IF
       .

       *> Recorre la lista y busca una marca
       BUSCAR-UNA-MARCA.
         INITIALIZE  ALGUNA-MARCA
         MOVE "N" TO ALGUNA-MARCA

         INITIALIZE REG-FEEEEVAR
         MOVE "BAN" TO COD-REG-VAR
         MOVE 1 TO CLAVE-VAR
         CALL "ST-VAR" USING REG-FEEEEVAR, ">=", CLAVE-VAR, FCD-ERROR
         IF ERROR-1 <> "0" AND FCD-ERROR <> "23" THEN
            MOVE "N" TO OP-EXITO
            PERFORM  PRESENTA-ERROR
         END-IF
         IF ERROR-1 = "0" OR FCD-ERROR = "9D" THEN
            CALL "RN-VAR" USING "N", FCD-ERROR
            IF ERROR-1 = "0" OR FCD-ERROR = "9D" THEN
               PERFORM UNTIL (ERROR-1 <> "0" AND FCD-ERROR <> "9D") OR
                             COD-REG-VAR <> "BAN" OR
                             ALGUNA-MARCA = "S"
                 IF (ERROR-1 = "0" OR FCD-ERROR = "9D") AND
                    "BAN" = COD-REG-VAR THEN
                    IF OMI-BAN-VAR = "S" THEN
                       MOVE "S" TO ALGUNA-MARCA
                    ELSE
                       CALL "RN-VAR" USING "N", FCD-ERROR
                    END-IF
                 END-IF
               END-PERFORM
            ELSE
              MOVE "N" TO OP-EXITO
              PERFORM  PRESENTA-ERROR
            END-IF
         END-IF
       .

       *> Se encarga de buscar una marca por lo menos
      *PONER-UNA-MARCA.
      *  PERFORM BUSCAR-UNA-MARCA
      *  IF ALGUNA-MARCA = "N" THEN
      *     PERFORM MARCAR-PRIMERO
      *     PERFORM MENSAJE-NO-MARCAS
      *  END-IF
      *.

       *> -------------------------------------------------------------

       *> TRATAMIENTO DE FICHEROS -------------------------------------
       Modulo-TFI.
         initialize NP-ECOM-TFI
                    CODIGO-FTO

         CALL "ECOMTFI" USING NP-ECOM-TFI,
                              TABLA-OPERACIONES,
                              CODIGO-FTO,
                              MES-FICHERO,
                              ECO-ACT-COD,
                              OP-EXITO
       .

       *> Reorganiza la lista sin dejar huecos en medio de COD-BAN-BCP
       REORGANIZAR-LISTA.
         INITIALIZE COD-ACT-VAR-TEMPOR
         INITIALIZE REG-FEEEEVAR
         MOVE "BAN" TO COD-REG-VAR
         MOVE 1 TO CLAVE-VAR
         CALL "ST-VAR" USING REG-FEEEEVAR, ">=", CLAVE-VAR, FCD-ERROR
         IF ERROR-1 <> "0" AND FCD-ERROR <> "23" THEN
            MOVE "N" TO OP-EXITO
            PERFORM  PRESENTA-ERROR
         END-IF

         INITIALIZE CONTADOR
         MOVE 0 TO CONTADOR

         PERFORM UNTIL (ERROR-1 <> "0" AND FCD-ERROR <> "9D") OR
                       CONTADOR>=Max-Bancos OR
                       COD-REG-VAR <> "BAN"
           CALL "RN-VAR" USING "N", FCD-ERROR
           IF FCD-ERROR = "00" AND "BAN" = COD-REG-VAR THEN
              ADD 1 TO CONTADOR
              IF COD-ACT-VAR <> CONTADOR THEN
                 MOVE COD-ACT-VAR TO COD-ACT-VAR-TEMPOR
                 MOVE CONTADOR TO COD-ACT-VAR
                 CALL "WR-VAR" USING FCD-ERROR
                 IF ERROR-1 = "0" OR FCD-ERROR = "9D" THEN
                    MOVE COD-ACT-VAR-TEMPOR TO COD-ACT-VAR
                    CALL "DE-VAR" USING FCD-ERROR
                 ELSE
                    MOVE "N" TO OP-EXITO
                    PERFORM  PRESENTA-ERROR
                 END-IF
              END-IF
           END-IF
         END-PERFORM
       .

       *> -------------------------------------------------------------

       *> Cargamos la lista de entry-fields
       Cargar-EntryFields.
         IF ECO9BAN-ITEM-LISTA > 0 THEN
            INITIALIZE REG-FEEEEVAR
            MOVE "BAN" TO COD-REG-VAR
            MOVE ECO9BAN-COD-BAN-LISTA(ECO9BAN-ITEM-LISTA) TO
                 COD-ACT-VAR
            MOVE 1 TO CLAVE-VAR
            CALL "ST-VAR" USING REG-FEEEEVAR, ">=", CLAVE-VAR, FCD-ERROR
            IF ERROR-1 = "0" THEN
               CALL "RN-VAR" USING "N", FCD-ERROR
               IF ERROR-1 = "0" OR FCD-ERROR = "9D" AND
                  ECO9BAN-COD-BAN-LISTA(ECO9BAN-ITEM-LISTA)=COD-ACT-VAR
                  AND COD-REG-VAR = "BAN" THEN
                  PERFORM DATOS-FICHERO-TO-PANTALLA
                  IF OMI-BAN-VAR = "S" THEN
                     MOVE 1 TO ECO9BAN-OMI-BAN-VAR
                     MOVE "S" TO ERA-PREDET
                  ELSE
                     MOVE 0 TO ECO9BAN-OMI-BAN-VAR
                     MOVE "N" TO ERA-PREDET
                  END-IF
               END-IF
            ELSE
              IF FCD-ERROR <> "23" THEN
                 MOVE "N" TO OP-EXITO
                 PERFORM  PRESENTA-ERROR
              END-IF
            END-IF
         END-IF
       .

       *> Modifica el registro actual.
       MODI-REGISTRO.
         INITIALIZE REG-FEEEEVAR
         MOVE "BAN" TO COD-REG-VAR
         MOVE ECO9BAN-COD-BAN-LISTA(ECO9BAN-ITEM-LISTA)
              TO COD-ACT-VAR
         MOVE 1 TO CLAVE-VAR
         CALL "ST-VAR" USING REG-FEEEEVAR, ">=", CLAVE-VAR, FCD-ERROR
         IF ERROR-1 = "0" THEN
            CALL "RN-VAR" USING "N", FCD-ERROR
            CALL "DE-VAR" USING FCD-ERROR
            PERFORM DATOS-PANTALLA-TO-FICHERO
            CALL "WR-VAR" USING FCD-ERROR
      *     IF ECO9BAN-OMI-BAN-VAR = 0 THEN
      *        IF ERA-PREDET = "S"
      *           PERFORM MENSAJE-PREDETERMINADO
      *           PERFORM MARCAR-PRIMERO
      *        END-IF
      *     END-IF
            IF OMI-BAN-VAR = "S" THEN
               PERFORM REORGANIZAR-MARCAS
            END-IF
         ELSE
           IF FCD-ERROR <> "23" THEN
              MOVE "N" TO OP-EXITO
              PERFORM  PRESENTA-ERROR
           END-IF
         END-IF
         *>PERFORM PONER-UNA-MARCA
       .

       *> MENSAJES ----------------------------------------------------
       *> Máximo de 100 bancos
       MENSAJE-MAX-100.
         MOVE "Atención" to stawmsg-titulo
         *> VICTOR - Revisar si es cliente o proveedor en el mensaje
         MOVE "Solo se permiten 100 bancos por cliente."
              TO stawmsg-texto
         *> VICTOR
         move stawmsg-ok  to stawmsg-boton
         move stawmsg-information  to stawmsg-icono
         call "STAWMSG" using np-stawmsg stawmsg-lkg
         cancel "STAWMSG"
       .

       *> Duplicados
       MENSAJE-DUPLICADOS.
         MOVE "Atención" to stawmsg-titulo
         MOVE "No se pueden repetir bancos ."
              TO stawmsg-texto
         move stawmsg-ok  to stawmsg-boton
         move stawmsg-information  to stawmsg-icono
         call "STAWMSG" using np-stawmsg stawmsg-lkg
         cancel "STAWMSG"
       .

       *> No hay ningún banco predeterminado
       MENSAJE-NO-MARCAS.
         MOVE "Atención" to stawmsg-titulo
         MOVE "No hay seleccionada ninguna cuenta por omisión.\n"  &
              "Se marcará la primera cuenta de la lista como cuenta " &
              "por omisión."
              TO stawmsg-texto
         move stawmsg-ok  to stawmsg-boton
         move stawmsg-information  to stawmsg-icono
         call "STAWMSG" using np-stawmsg stawmsg-lkg
         cancel "STAWMSG"
       .

       *> Mensaje no hay elementos predeterminados
      *MENSAJE-PREDETERMINADO.
      *  MOVE "Atención" to stawmsg-titulo
      *  MOVE "Ha desmarcado el indicador de cuenta por omisión.\n"  &
      *       "Se marcará la primera cuenta de la lista como cuenta " &
      *       "por omisión."
      *       TO stawmsg-texto
      *  move stawmsg-ok  to stawmsg-boton
      *  move stawmsg-information  to stawmsg-icono
      *  call "STAWMSG" using np-stawmsg stawmsg-lkg
      *  cancel "STAWMSG"
      *.

       *> Mensaje no hay elementos predeterminados
       MENSAJE-PREDETERMINADO-BAJA.
         MOVE "Atención" to stawmsg-titulo
         MOVE "Ha borrado una cuenta con el indicador por omisión.\n"  &
              "Se marcará la primera cuenta de la lista como cuenta " &
              "por omisión."
              TO stawmsg-texto
         move stawmsg-ok  to stawmsg-boton
         move stawmsg-information  to stawmsg-icono
         call "STAWMSG" using np-stawmsg stawmsg-lkg
         cancel "STAWMSG"
       .

       *> Al validar campos no se puede dejar ni la entidad, ni la ofic.
       *> ni el nº cuenta vacios.
       MENSAJE-NO-CEROS.
         MOVE "Atención" to stawmsg-titulo
      *  MOVE "Los campos Entidad, Nº Cuenta y Cuenta " &
         MOVE "Los campos Entidad y Cuenta " &
              "no pueden estar vacíos."
              TO stawmsg-texto
         move stawmsg-ok  to stawmsg-boton
         move stawmsg-information  to stawmsg-icono
         call "STAWMSG" using np-stawmsg stawmsg-lkg
         cancel "STAWMSG"
       .

       *> ¿ Seguro que desea borrar este registro ?
       MENSAJE-CONFIRMAR-BAJA.
         MOVE "Atención" to stawmsg-titulo
         MOVE "Ha indicado eliminar la cuenta seleccionada.\n" &
              "¿ Desea continuar ?"
              TO stawmsg-texto
         move stawmsg-yesno  to stawmsg-boton
         move stawmsg-information  to stawmsg-icono
         call "STAWMSG" using np-stawmsg stawmsg-lkg
         cancel "STAWMSG"
       .

       *> No hay bancos quiero darlos de alta
       MENSAJE-NO-BANCOS.
         MOVE "Atención" to stawmsg-titulo
         MOVE "La tabla de bancos de la empresa está vacia.\n" &
              "Para continuar debería tener configurado al menos un " &
              "banco.\n" &
              "¿ Desea darlo de alta ahora ?"
              TO stawmsg-texto
         move stawmsg-yesno  to stawmsg-boton
         move stawmsg-information  to stawmsg-icono
         call "STAWMSG" using np-stawmsg stawmsg-lkg
         cancel "STAWMSG"
       .

       Mostrar-Mensaje.
         MOVE 2 TO NP-StawMsg
         CALL "STAWMSG" USING NP-StawMsg, StawMsg-LKG
         CANCEL "STAWMSG"
       .

       *> MÓDULO STABANCO ---------------------------------------------

       *> Mostramos la lista de los bancos
       F4-BANCOS.
           if a3Asesor-Integrado = "S"
              perform F4-Bco-A3Asesor
           else
              perform F4-Bco-A3ECO
           end-if
       .

       F4-Bco-A3Asesor.
           initialize ecoasope
           set EcoAsOpe-Ope-CCC-Sel to true
           call "EcoAsOpe" using EcoAsope
           cancel "EcoAsOpe"

           *> Esta condición se podrá eliminar
           *> a partir de la versión 1.1.2 de A3Asesor
           call var-estado-fichero using BANCOS stsFic
           if stsFic = "C"
              open i-o BANCOS
           end-if

           if EcoAsOpe-Sts-Ok
              move EcoAsOpe-Ccc(1:4)   to ECO9BAN-CC1-BAN-VAR
              move EcoAsOpe-Ccc(5:4)   to ECO9BAN-CC2-BAN-VAR
              move EcoAsOpe-Ccc(9:2)   to ECO9BAN-CC3-BAN-VAR
              move EcoAsOpe-Ccc(11:10) to ECO9BAN-CC4-BAN-VAR
              perform calcular-iban
              move "OK" to Eco9ban-Salida
              *> Cargar banco
              MOVE "B" TO FUNCION-BAN
              MOVE ECO9BAN-CC1-BAN-VAR TO CODIGO-BAN
              PERFORM MODULO-STABANCO
              IF TEC-PUL="CR"
                 MOVE CODIGO-BAN    TO ECO9BAN-CC1-BAN-VAR
                 MOVE NOMBRE-BAN    TO ECO9BAN-DES-CC1-BAN-VAR
                 MOVE "OK"          TO ECO9BAN-SALIDA
              end-if
           end-if
       .                                                                                                                                                                                                                                                      .

       F4-Bco-A3ECO.
           MOVE "L" TO FUNCION-BAN
           MOVE ECO9BAN-CC1-BAN-VAR TO CODIGO-BAN
           PERFORM MODULO-STABANCO
           IF TEC-PUL="CR"
              MOVE CODIGO-BAN    TO ECO9BAN-CC1-BAN-VAR
              MOVE NOMBRE-BAN    TO ECO9BAN-DES-CC1-BAN-VAR
              MOVE "OK"          TO ECO9BAN-SALIDA
           ELSE
              MOVE "B" TO FUNCION-BAN
              MOVE ECO9BAN-CC1-BAN-VAR TO CODIGO-BAN
              PERFORM MODULO-STABANCO
              IF TEC-PUL="CR"
                 MOVE CODIGO-BAN    TO ECO9BAN-CC1-BAN-VAR
                 MOVE NOMBRE-BAN    TO ECO9BAN-DES-CC1-BAN-VAR
                 MOVE "OK"          TO ECO9BAN-SALIDA
              ELSE
                 MOVE "NO" TO FUNCION-BAN
                 MOVE "Inexistente" TO ECO9BAN-DES-CC1-BAN-VAR
              END-IF
           END-IF
       .

       *> Le damos al tabulador de los bancos
       TAB-BANCOS.
           MOVE "B" TO FUNCION-BAN
           MOVE ECO9BAN-CC1-BAN-VAR TO CODIGO-BAN
           PERFORM MODULO-STABANCO
           IF TEC-PUL="CR"
              MOVE CODIGO-BAN    TO ECO9BAN-CC1-BAN-VAR
              MOVE NOMBRE-BAN    TO ECO9BAN-DES-CC1-BAN-VAR
              MOVE "OK"          TO ECO9BAN-SALIDA
           ELSE
              IF ECO9BAN-CC1-BAN-VAR <> 0 THEN
                 PERFORM F4-BANCOS
              ELSE
                 MOVE "OK" TO ECO9BAN-SALIDA
              END-IF
           END-IF
       .

       DESCRIPCION-BANCO.
         MOVE CC1-BAN-VAR TO CODIGO-BAN
         PERFORM CARGAR-DESCRIPCION
         MOVE NOMBRE-BAN
              TO ECO9BAN-DES-CC1-BAN-VAR
       .

       *> - ECOMCTA - Selección de cuentas -----------------------------

       MODULO-RELACION-CUENTAS.
           IF CUENTA=0 THEN
              MOVE 572000000000 TO CUENTA
           END-IF
           CALL "ECOMCTA" USING TIPO-BUSQUEDA,
                                MES-APUNTE,
                                CUENTA,
                                MODULO,
                                ECO-MONEDA-EMPRESA,
                                NRO-ORDEN,
                                TIPO-LINEA,
                                CLI-PRO,
                                COM-VEN,
                                SALDO-ACUMULADO,
                                SALIDA,
                                OP-EXITO
           CANCEL "ECOMCTA"
       .

       MODULO-ECOMNCT.
           CALL "ECOMNCT" USING NIVEL-ALTA,
                                CUENTA,
                                CLI-PRO,
                                COM-VEN,
                                SALDO-ACUMULADO,
                                SALIDA,
                                OP-EXITO,
                                ECO-MONEDA-EMPRESA
           CANCEL "ECOMNCT"
       .

       F4-CUENTAS.
         INITIALIZE Cuenta
         MOVE ECO9BAN-CTA-CON-VAR TO CUENTA
         if CUENTA = 0 OR CUENTA = SPACES
             MOVE 572000000000 TO ECO9BAN-CTA-CON-VAR
         end-if
         PERFORM MODULO-RELACION-CUENTAS
         IF SALIDA="CR" THEN
            MOVE CUENTA         TO ECO9BAN-CTA-CON-VAR
            MOVE DES-CTA-ACU    TO ECO9BAN-DES-CTA-CON-VAR
            MOVE "OK"           TO ECO9BAN-SALIDA
         ELSE
            MOVE "NO"           TO ECO9BAN-SALIDA
            MOVE 0              TO ECO9BAN-CTA-CON-VAR
            MOVE SPACES         TO ECO9BAN-DES-CTA-CON-VAR
         END-IF
       .


       *> Le damos al tabulador de las cuentas
       TAB-CUENTAS.
         IF ECO9BAN-CTA-CON-VAR <> 0 THEN
            MOVE "CT" TO NIVEL-ALTA
            MOVE ECO9BAN-CTA-CON-VAR
                 TO CUENTA
            PERFORM MODULO-ECOMNCT
            IF SALIDA = "CR" OR SALIDA = "F3" THEN
               MOVE CUENTA         TO ECO9BAN-CTA-CON-VAR
               MOVE DES-CTA-ACU    TO ECO9BAN-DES-CTA-CON-VAR
               MOVE "OK"           TO ECO9BAN-SALIDA
            ELSE
               INITIALIZE          ECO9BAN-CTA-CON-VAR
               MOVE "NO"           TO ECO9BAN-SALIDA
               *>PERFORM F4-CUENTAS
            END-IF
         ELSE
            MOVE "OK"              TO ECO9BAN-SALIDA
            MOVE SPACES            TO ECO9BAN-DES-CTA-CON-VAR
         END-IF
       .

       *> Le damos al tabulador de las cuentas
       CARGAR-DESCRIPCION-CUENTA.
         MOVE "CT" TO NIVEL-ALTA
         PERFORM MODULO-ECOMNCT
       .

       DESCRIPCION-CUENTA.
         IF ECO9BAN-CTA-CON-VAR <> 0 THEN
            *> Antes de cargar la descripción debería
            MOVE CTA-CON-VAR TO CUENTA
            PERFORM CARGAR-DESCRIPCION-CUENTA
            IF SALIDA = "CR" OR SALIDA = "F3" AND OP-EXITO = "S" THEN
               MOVE CUENTA         TO ECO9BAN-CTA-CON-VAR
               MOVE DES-CTA-ACU    TO ECO9BAN-DES-CTA-CON-VAR
            ELSE
               MOVE SPACES         TO ECO9BAN-DES-CTA-CON-VAR
            END-IF
         ELSE
            MOVE "OK"              TO ECO9BAN-SALIDA
            MOVE SPACES            TO ECO9BAN-DES-CTA-CON-VAR
         END-IF
       .

       *> MASCARAS -----------------------------------------------------

       PONER-MASCARAS.
         CALL "PONER_MASCARA" USING ECO9BAN-HANDLE-CUENTA, ECO-DIGITOS
       .

       *> BANCA ELECTRÓNICA --------------------------------------------

       GRABAR-BANCA.
         *> - PARTE DE BANCA ELECTRÓNICA ...
         INITIALIZE REG-STABANEL
         MOVE CC1-BAN-VAR TO COD-BAN-BAE
         START STABANEL KEY IS = CLV-BAE-1
         MOVE ECO9BAN-IND-BAN-BAE TO IND-BAN-BAE
         MOVE ECO9BAN-DIR-WWW-BAE TO DIR-WWW-BAE
         MOVE ECO9BAN-DIR-LOC-BAE TO DIR-LOC-BAE
         MOVE ECO9BAN-DIR-FIC-BAE TO DIR-FIC-BAE
         IF ERROR-1 = "0" THEN
            REWRITE REG-STABANEL
         ELSE
            WRITE REG-STABANEL
         END-IF
       .

       CARGAR-BANCA.
         *> - PARTE DE BANCA ELECTRÓNICA ...
         INITIALIZE REG-STABANEL
         *> VICTOR
      *  MOVE CC1-BAN-VAR TO COD-BAN-BAE
         MOVE ECO9BAN-CC1-BAN-VAR TO COD-BAN-BAE
         MOVE "I"         TO ECO9BAN-IND-BAN-BAE
         MOVE SPACES      TO ECO9BAN-DIR-WWW-BAE
         MOVE SPACES      TO ECO9BAN-DIR-LOC-BAE
         MOVE COD-BAN-BAE TO COD-BAN-BAE-TMP
         STRING "C:\BUZON\AEB",COD-BAN-BAE-TMP
                          INTO ECO9BAN-DIR-FIC-BAE

         START STABANEL KEY IS = CLV-BAE-1
         IF ERROR-1 = "0" THEN
            READ STABANEL NEXT
            IF ERROR-1 = "0" OR FCD-ERROR = "9D" THEN
               MOVE IND-BAN-BAE TO ECO9BAN-IND-BAN-BAE
               IF IND-BAN-BAE = "I" OR IND-BAN-BAE = SPACES THEN
                  MOVE DIR-WWW-BAE TO ECO9BAN-DIR-WWW-BAE
                  MOVE SPACES      TO ECO9BAN-DIR-LOC-BAE
               ELSE
                  MOVE SPACES      TO ECO9BAN-DIR-WWW-BAE
                  MOVE DIR-LOC-BAE TO ECO9BAN-DIR-LOC-BAE
               END-IF
               IF DIR-FIC-BAE = SPACES THEN
                  MOVE COD-BAN-BAE TO COD-BAN-BAE-TMP
                  STRING "C:\BUZON\AEB",COD-BAN-BAE-TMP
                         INTO ECO9BAN-DIR-FIC-BAE
               ELSE
                  MOVE DIR-FIC-BAE TO ECO9BAN-DIR-FIC-BAE
               END-IF
            END-IF
         ELSE
           IF FCD-ERROR <> "23" THEN
              MOVE "N" TO OP-EXITO
              PERFORM PRESENTA-ERROR
           END-IF
         END-IF
       .

       *> ----------------------------------------------------------
       F4-MAIL.
         INITIALIZE DIRECCION-INTERNET
         INITIALIZE ASUNTO-MAIL
         INITIALIZE FICHERO-INTERNET
         MOVE ECO9BAN-MAIL-BAN-VAR TO DIRECCION-INTERNET
         MOVE SPACES               TO ASUNTO-MAIL
         MOVE SPACES               TO FICHERO-INTERNET
         CALL "ECOMAIL" USING DIRECCION-INTERNET,
                              ASUNTO-MAIL,
                              FICHERO-INTERNET
         CANCEL "ECOMAIL"
       .

       F4-PROVINCIA.
         INITIALIZE CODIGO-F4
         MOVE ECO9BAN-CPO-BAN-VAR(1:2) TO CODIGO-F4(1:2)
         MOVE 4 TO NP-STAPROV
         PERFORM MODULO-STAPROV
         IF F4-TROBAT="S" OR NP-STAPROV=3
             MOVE CODIGO-F4(1:2) TO ECO9BAN-CPO-BAN-VAR(1:2)
             MOVE DESC-F4 TO ECO9BAN-DES-CPO-BAN-VAR
         END-IF
       .

       TAB-PROVINCIA.
         INITIALIZE CODIGO-F4
         MOVE ECO9BAN-CPO-BAN-VAR(1:2) TO CODIGO-F4(1:2)
         MOVE 3 TO NP-STAPROV
         PERFORM MODULO-STAPROV
         IF F4-TROBAT="S" OR NP-STAPROV=3
             MOVE CODIGO-F4(1:2) TO ECO9BAN-CPO-BAN-VAR(1:2)
             MOVE DESC-F4 TO ECO9BAN-DES-CPO-BAN-VAR
         END-IF
       .

       DESCRIPCION-PROVINCIA.
         INITIALIZE CODIGO-F4
         MOVE ECO9BAN-CPO-BAN-VAR(1:2) TO CODIGO-F4(1:2)
         MOVE 3 TO NP-STAPROV
         PERFORM MODULO-STAPROV
         IF F4-TROBAT="S" OR NP-STAPROV=3
             MOVE CODIGO-F4(1:2) TO ECO9BAN-CPO-BAN-VAR(1:2)
             MOVE DESC-F4 TO ECO9BAN-DES-CPO-BAN-VAR
         Else
            MOVE "Inexistente" TO ECO9BAN-DES-CTA-CON-VAR
         END-IF
       .


       MODULO-STAPROV.
           CALL "STAPROV" USING NP-STAPROV,
                                ECO-CAMINO-ACCESO-ENTORNO,
                                CLAVE-F4,
                                CTRL-SOMBRA,
                                NP-STASOMBR,
                                NP-STAWERR,
                                NP-STACALCW,
                                NP-STAHELPW
           CANCEL "STAPROV"
       .

       *> --------------------------------------------------------------

       F4-LOCAL.
         move 3 to np-stafile
         move stf-funcion-abrir to stafile-funcion
         initialize stafile-linkage
         move "Búsqueda del programa de banca electrónica"
              to stf-Titulo-Ventana
         move ECO9BAN-HANDLE-VENTANA to stf-Handle-Padre
         move ECO9BAN-DIR-LOC-BAE to stf-Camino-Inicial
         move "Ejecutables (*.EXE, *.COM, *.BAT)" to stf-flt-nombre(1)
         move "*.EXE;*.COM;*.BAT" to stf-flt-mascara(1)
         move "Todos los Archivos (*.*)" to stf-flt-nombre(2)
         move "*.*" to stf-flt-mascara(2)
         move "S" to stf-Debe-Existir
         move "N" to stf-Nombre-Corto
         call "STAFILE" using np-stafile, stafile-funcion,
                              stafile-linkage
         if stf-retorno = stf-Ok then
           move stf-Nombre-Archivo to ECO9BAN-DIR-LOC-BAE
         else
           *> stf-retorno = stf-Cancel
           CONTINUE
         end-if
      *  MOVE ECO9BAN-DIR-LOC-BAE TO CAMINO-ACCESO-DIR
      *  MOVE 4 TO NP-STADIREC
      *  CALL "STADIREC" USING NP-STADIREC,
      *                        CAMINO-ACCESO-DIR,
      *                        TECLA-PULSADA,
      *                        OP-EXITO
      *  CANCEL "STADIREC"
      *  IF OP-EXITO = "S" AND TECLA-PULSADA = "CR"
      *     MOVE CAMINO-ACCESO-DIR TO ECO9BAN-DIR-LOC-BAE
      *  END-IF.
       .
       *> --------------------------------------------------------------

       F4-CSB.
         MOVE ECO9BAN-DIR-FIC-BAE TO CAMINO-ACCESO-DIR
         MOVE 4 TO NP-STADIREC
         CALL "STADIREC" USING NP-STADIREC,
                               CAMINO-ACCESO-DIR,
                               TECLA-PULSADA,
                               OP-EXITO
         CANCEL "STADIREC"
         IF OP-EXITO = "S" AND TECLA-PULSADA = "CR"
            MOVE CAMINO-ACCESO-DIR TO ECO9BAN-DIR-FIC-BAE
         END-IF.
       .

       *> --- FUNCIONES EXTERNAS --------------------------------------

       EVALUAR-FUNCION.
         EVALUATE FUNCION
           WHEN "OO" *> Obtener la cuenta por omisión
             PERFORM OBTENER-OMISION
           WHEN "OD" *> Obtener datos
             PERFORM OBTENER-DATOS
           WHEN "OC" *> Obtener datos de una cuenta de tesoreria
             PERFORM OBTENER-CUENTA-TESORERIA
           when "TA" *> Obtener tabla
             perform Cargar-Tabla
             if  Numero-Bancos-Lkg = 0
               perform Mensaje-No-Bancos
               if Stawmsg-Retorno <> Stawmsg-Return-Yes
                  move "S" to Fin-Programa
                  MOVE "ES" TO SALIDA
               end-if
             else
               move "S" to Fin-Programa *> Para que no salga de pantalla
             end-if
           when "TM" *> Obtener tabla
             perform Cargar-Tabla
             move "S" to Fin-Programa *> Para que no salga de pantalla
           when "MD" *> Mantenimiento
               move "MA" to Eco9ban-Funcion

           when "F4" *> F4 de bancos de la empresa
               move "MC" to Eco9ban-Funcion

           when "AL" *> Alta de cuenta
               move "ES" to Salida

               *> primero cargamos las variables de Screenset
               move Cta-Banco-Lkg(1) to eco9ban-cta-con-var
               move CC1-BAN-VAR-LKG(1) to eco9ban-cc1-ban-var
               move CC2-BAN-VAR-LKG(1) to eco9ban-cc2-ban-var
               move CC3-BAN-VAR-LKG(1) to eco9ban-cc3-ban-var
               move CC4-BAN-VAR-LKG(1) to eco9ban-cc4-ban-var
               move Nombre-Oficina-Lkg(1) to eco9ban-ofi-ban-var
               move Domicilio-Oficina-Lkg(1) to eco9ban-dom-ban-var
               initialize eco9ban-omi-ban-var
                          eco9ban-mun-ban-var
                          eco9ban-cpo-ban-var
                          eco9ban-tlf-ban-var
                          eco9ban-fax-ban-var
                          eco9ban-per-ban-var
                          eco9ban-mail-ban-var

               *> Realizamos los validaciones
               move "ALTA" to Eco9Ban-Modo
               perform Comprobar-Cuenta-Repetida

               if Eco9Ban-Salida <> "NO"
                  *> Si todo ha ido correcto grabamos y
                  *> Salimos
                  perform Alta-Ban
                  move "CR" to Salida
                  move "S" to Op-Exito
               else
                  if Cuenta-Repetida = "S"
                     move "C" to Op-Exito
                  end-if

                  if CCC-Repetida = "S"
                     move "B" to Op-Exito
                  end-if
               end-if

               move "S" to Fin-Programa

         END-EVALUATE
         if Fin-Programa not = "S" and A3Asesor-Integrado = "S"
               perform VerAser-CCC
         end-if
       .

       OBTENER-OMISION.
         MOVE "S" TO FIN-PROGRAMA *> Para que no salga la pantalla.

         INITIALIZE  ALGUNA-MARCA
         MOVE "N" TO ALGUNA-MARCA

         INITIALIZE REG-FEEEEVAR
         MOVE "BAN" TO COD-REG-VAR
         MOVE 1 TO CLAVE-VAR
         CALL "ST-VAR" USING REG-FEEEEVAR, ">=", CLAVE-VAR, FCD-ERROR
         IF ERROR-1 <> "0" AND FCD-ERROR <> "23" THEN
            MOVE "N" TO OP-EXITO
            PERFORM  PRESENTA-ERROR
         END-IF
         CALL "RN-VAR" USING "N", FCD-ERROR
         PERFORM UNTIL (ERROR-1 <> "0" AND FCD-ERROR <> "9D") OR
                       COD-REG-VAR <> "BAN" OR
                       ALGUNA-MARCA = "S"
           IF (ERROR-1 = "0" OR FCD-ERROR = "9D") AND
              "BAN" = COD-REG-VAR THEN
              IF OMI-BAN-VAR = "S" THEN
                 MOVE "S" TO ALGUNA-MARCA
                 MOVE "CR" TO SALIDA
                 MOVE CTA-CON-VAR TO CTA-CON-VAR-LKG
                 MOVE COD-ACT-VAR TO COD-ACT-VAR-TEMPOR
                 MOVE COD-ACT-VAR-TEMPOR(1:2) TO COD-ACT-VAR-LKG
                 MOVE CC1-BAN-VAR TO CC1-BAN-VAR-LKG(1)
                 MOVE CC2-BAN-VAR TO CC2-BAN-VAR-LKG(1)
                 MOVE CC3-BAN-VAR TO CC3-BAN-VAR-LKG(1)
                 MOVE CC4-BAN-VAR TO CC4-BAN-VAR-LKG(1)

                 *> Datos de la oficina
                 MOVE OFI-BAN-VAR TO Nombre-Oficina-Lkg(1)
                 MOVE Dom-BAN-VAR TO Domicilio-Oficina-Lkg(1)
              ELSE
                 CALL "RN-VAR" USING "N", FCD-ERROR
              END-IF
           END-IF
         END-PERFORM
         IF ALGUNA-MARCA <> "S" THEN
            MOVE "ES" TO SALIDA
         END-IF
       .

       OBTENER-DATOS.
         MOVE "S" TO FIN-PROGRAMA *> Para que no salga la pantalla.

         INITIALIZE  ALGUNA-MARCA
         MOVE "N" TO ALGUNA-MARCA

         INITIALIZE REG-FEEEEVAR
         MOVE "BAN" TO COD-REG-VAR
         MOVE 1 TO CLAVE-VAR
         CALL "ST-VAR" USING REG-FEEEEVAR, ">=", CLAVE-VAR, FCD-ERROR
         IF ERROR-1 <> "0" AND FCD-ERROR <> "23" THEN
            MOVE "N" TO OP-EXITO
            PERFORM  PRESENTA-ERROR
         END-IF
         CALL "RN-VAR" USING "N", FCD-ERROR
         PERFORM UNTIL (ERROR-1 <> "0" AND FCD-ERROR <> "9D") OR
                       COD-REG-VAR <> "BAN" OR
                       ALGUNA-MARCA = "S"
           IF (ERROR-1 = "0" OR FCD-ERROR = "9D") AND
              "BAN" = COD-REG-VAR THEN
              IF CCC-BAN-VAR = CCC-BAN-VAR-LKG(1) THEN
                 MOVE "S" TO ALGUNA-MARCA
                 MOVE "CR" TO SALIDA
                 MOVE CTA-CON-VAR TO CTA-CON-VAR-LKG
                                     Cta-Banco-Lkg(1)
                 MOVE COD-ACT-VAR TO COD-ACT-VAR-TEMPOR
                 MOVE COD-ACT-VAR-TEMPOR(1:2) TO COD-ACT-VAR-LKG
                 MOVE CC1-BAN-VAR TO CC1-BAN-VAR-LKG(1)
                 MOVE CC2-BAN-VAR TO CC2-BAN-VAR-LKG(1)
                 MOVE CC3-BAN-VAR TO CC3-BAN-VAR-LKG(1)
                 MOVE CC4-BAN-VAR TO CC4-BAN-VAR-LKG(1)

                 *> Datos de la oficina
                 MOVE OFI-BAN-VAR TO Nombre-Oficina-Lkg(1)
                 MOVE Dom-BAN-VAR TO Domicilio-Oficina-Lkg(1)

                 *> Buscar nombre de la entidad
                 move cc1-ban-var to codigo-ban
                 perform cargar-descripcion
                 move nombre-ban to nombre-entidad-lkg(1)
              ELSE
                 CALL "RN-VAR" USING "N", FCD-ERROR
              END-IF
           END-IF
         END-PERFORM
         IF ALGUNA-MARCA <> "S" THEN
            MOVE "ES" TO SALIDA
         END-IF
       .
       OBTENER-CUENTA-TESORERIA.
         MOVE "S" TO FIN-PROGRAMA *> Para que no salga la pantalla.

         INITIALIZE  ALGUNA-MARCA
         MOVE "N" TO ALGUNA-MARCA

         INITIALIZE REG-FEEEEVAR
         MOVE "BAN" TO COD-REG-VAR
         MOVE CTA-CON-VAR-LKG TO CTA-CON-VAR

         MOVE 1 TO CLAVE-VAR
         CALL "ST-VAR" USING REG-FEEEEVAR, ">=", CLAVE-VAR, FCD-ERROR
         IF ERROR-1 <> "0" AND FCD-ERROR <> "23" THEN
            MOVE "N" TO OP-EXITO
            PERFORM  PRESENTA-ERROR
         END-IF
         CALL "RN-VAR" USING "N", FCD-ERROR
         PERFORM UNTIL (ERROR-1 <> "0" AND FCD-ERROR <> "9D") OR
                       COD-REG-VAR <> "BAN" OR
                       ALGUNA-MARCA = "S"
           IF (ERROR-1 = "0" OR FCD-ERROR = "9D") AND
              "BAN" = COD-REG-VAR THEN
              IF CTA-CON-VAR = CTA-CON-VAR-LKG THEN
                 MOVE "S" TO ALGUNA-MARCA
                 MOVE "CR" TO SALIDA
                 MOVE COD-ACT-VAR TO COD-ACT-VAR-TEMPOR
                 MOVE COD-ACT-VAR-TEMPOR(1:2) TO COD-ACT-VAR-LKG
                 MOVE CC1-BAN-VAR TO CC1-BAN-VAR-LKG(1)
                 MOVE CC2-BAN-VAR TO CC2-BAN-VAR-LKG(1)
                 MOVE CC3-BAN-VAR TO CC3-BAN-VAR-LKG(1)
                 MOVE CC4-BAN-VAR TO CC4-BAN-VAR-LKG(1)

                 *> Datos de la oficina
                 MOVE OFI-BAN-VAR TO Nombre-Oficina-Lkg(1)
                 MOVE Dom-BAN-VAR TO Domicilio-Oficina-Lkg(1)

                 *> Buscar nombre de la entidad
                 move cc1-ban-var to codigo-ban
                 perform cargar-descripcion
                 move nombre-ban to nombre-entidad-lkg(1)
              ELSE
                 CALL "RN-VAR" USING "N", FCD-ERROR
              END-IF
           END-IF
         END-PERFORM
         IF ALGUNA-MARCA <> "S" THEN
            MOVE "ES" TO SALIDA
         END-IF
       .

       OBTENER-CSB.
         STRING "C:\BUZON\AEB",ECO9BAN-CC1-BAN-VAR
                          INTO ECO9BAN-DIR-FIC-BAE
       .
       *> ----------------- GRABACIÓN POR PARTES -----------------------

       MODIFICAR-ELE.
         IF ECO9BAN-CC1-BAN-VAR <> 0 THEN
            MOVE ECO9BAN-CC1-BAN-VAR TO CC1-BAN-VAR
            PERFORM GRABAR-BANCA
         END-IF
       .

       MODIFICAR-BAN.
         INITIALIZE REG-FEEEEVAR
         MOVE "BAN" TO COD-REG-VAR
         MOVE ECO9BAN-COD-BAN-LISTA(ECO9BAN-ITEM-LISTA)
              TO COD-ACT-VAR
         MOVE 1 TO CLAVE-VAR
         CALL "ST-VAR" USING REG-FEEEEVAR, ">=", CLAVE-VAR, FCD-ERROR
         IF ERROR-1 = "0" THEN
            CALL "RN-VAR" USING "N", FCD-ERROR
            CALL "DE-VAR" USING FCD-ERROR

            MOVE ECO9BAN-CTA-CON-VAR       TO CTA-CON-VAR
            MOVE ECO9BAN-CC1-BAN-VAR       TO CC1-BAN-VAR
            MOVE ECO9BAN-CC2-BAN-VAR       TO CC2-BAN-VAR
            MOVE ECO9BAN-CC3-BAN-VAR       TO CC3-BAN-VAR
            MOVE ECO9BAN-CC4-BAN-VAR       TO CC4-BAN-VAR
            IF ECO9BAN-OMI-BAN-VAR = 1 THEN
               MOVE "S" TO OMI-BAN-VAR
            ELSE
               MOVE "N" TO OMI-BAN-VAR
            END-IF
            MOVE ECO9BAN-OFI-BAN-VAR       TO OFI-BAN-VAR
            MOVE ECO9BAN-DOM-BAN-VAR       TO DOM-BAN-VAR
            MOVE ECO9BAN-MUN-BAN-VAR       TO MUN-BAN-VAR
            MOVE ECO9BAN-CPO-BAN-VAR       TO CPO-BAN-VAR
            MOVE ECO9BAN-TLF-BAN-VAR       TO TLF-BAN-VAR
            MOVE ECO9BAN-FAX-BAN-VAR       TO FAX-BAN-VAR
            MOVE ECO9BAN-PER-BAN-VAR       TO PER-BAN-VAR
            MOVE ECO9BAN-MAIL-BAN-VAR      TO MAIL-BAN-VAR

            CALL "WR-VAR" USING FCD-ERROR
      *     IF ECO9BAN-OMI-BAN-VAR = 0 THEN
      *        IF ERA-PREDET = "S"
      *           PERFORM MENSAJE-PREDETERMINADO
      *           PERFORM MARCAR-PRIMERO
      *        END-IF
      *     END-IF
            IF OMI-BAN-VAR = "S" THEN
               PERFORM REORGANIZAR-MARCAS
            END-IF
         ELSE
            IF FCD-ERROR <> "23" THEN
               MOVE "N" TO OP-EXITO
               PERFORM  PRESENTA-ERROR
            END-IF
         END-IF
       .

       ALTA-BAN.
         PERFORM BUSCAR-BANCO-LIBRE

         INITIALIZE REG-FEEEEVAR
         MOVE "BAN" TO COD-REG-VAR
         MOVE COD-PROX-BAN    TO COD-ACT-VAR

         MOVE ECO9BAN-CTA-CON-VAR       TO CTA-CON-VAR
         MOVE ECO9BAN-CC1-BAN-VAR       TO CC1-BAN-VAR
         MOVE ECO9BAN-CC2-BAN-VAR       TO CC2-BAN-VAR
         MOVE ECO9BAN-CC3-BAN-VAR       TO CC3-BAN-VAR
         MOVE ECO9BAN-CC4-BAN-VAR       TO CC4-BAN-VAR
         IF ECO9BAN-OMI-BAN-VAR = 1 THEN
            MOVE "S" TO OMI-BAN-VAR
         ELSE
            MOVE "N" TO OMI-BAN-VAR
         END-IF
         MOVE ECO9BAN-OFI-BAN-VAR       TO OFI-BAN-VAR
         MOVE ECO9BAN-DOM-BAN-VAR       TO DOM-BAN-VAR
         MOVE ECO9BAN-MUN-BAN-VAR       TO MUN-BAN-VAR
         MOVE ECO9BAN-CPO-BAN-VAR       TO CPO-BAN-VAR
         MOVE ECO9BAN-TLF-BAN-VAR       TO TLF-BAN-VAR
         MOVE ECO9BAN-FAX-BAN-VAR       TO FAX-BAN-VAR
         MOVE ECO9BAN-PER-BAN-VAR       TO PER-BAN-VAR
         MOVE ECO9BAN-MAIL-BAN-VAR      TO MAIL-BAN-VAR

         CALL "WR-VAR" USING FCD-ERROR
         IF OMI-BAN-VAR = "S" THEN
            PERFORM REORGANIZAR-MARCAS
         END-IF
      *  PERFORM PONER-UNA-MARCA
       .

       COMPROBAR-CUENTA-REPETIDA.
         INITIALIZE CUENTA-REPETIDA
         INITIALIZE CCC-REPETIDA
         MOVE "N" TO CUENTA-REPETIDA
         MOVE "N" TO CCC-REPETIDA

         INITIALIZE COD-ACT-VAR-TEMPOR
         MOVE COD-ACT-VAR TO COD-ACT-VAR-TEMPOR

         INITIALIZE REG-FEEEEVAR
         MOVE "BAN"           TO COD-REG-VAR
         MOVE 1               TO CLAVE-VAR
         MOVE SPACES          TO COD-ACT-VAR
         CALL "ST-VAR" USING REG-FEEEEVAR, ">=", CLAVE-VAR, FCD-ERROR
         IF ERROR-1 = "0" OR FCD-ERROR = "9D" THEN
            PERFORM UNTIL (ERROR-1 <> "0" AND FCD-ERROR <> "9D") OR
                          CONTADOR>=Max-Bancos OR
                          COD-REG-VAR <> "BAN"
              CALL "RN-VAR" USING "N", FCD-ERROR
              IF (ERROR-1 = "0" OR FCD-ERROR = "9D") AND
                 COD-REG-VAR = "BAN" THEN
                 MOVE COD-ACT-VAR TO COD-BAN-LISTA
                 IF ECO9BAN-MODO = "ALTA" THEN
                    IF (CTA-CON-VAR = ECO9BAN-CTA-CON-VAR) THEN
                       MOVE "S"  TO CUENTA-REPETIDA
                       MOVE "NO" TO ECO9BAN-SALIDA
                    END-IF
                    IF (CC1-BAN-VAR = ECO9BAN-CC1-BAN-VAR AND
                        CC2-BAN-VAR = ECO9BAN-CC2-BAN-VAR AND
                        CC3-BAN-VAR = ECO9BAN-CC3-BAN-VAR AND
                        CC4-BAN-VAR = ECO9BAN-CC4-BAN-VAR) THEN
                        MOVE "S"  TO CCC-REPETIDA
                        MOVE "NO" TO ECO9BAN-SALIDA
                    END-IF
                 END-IF
                 IF ECO9BAN-MODO = "MODI" THEN
                    IF (CTA-CON-VAR = ECO9BAN-CTA-CON-VAR) AND
                       (COD-BAN-LISTA <>
                        ECO9BAN-COD-BAN-LISTA(ECO9BAN-ITEM-LISTA))
                       MOVE "S"  TO CUENTA-REPETIDA
                       MOVE "NO" TO ECO9BAN-SALIDA
                    END-IF
                    IF (CC1-BAN-VAR = ECO9BAN-CC1-BAN-VAR AND
                        CC2-BAN-VAR = ECO9BAN-CC2-BAN-VAR AND
                        CC3-BAN-VAR = ECO9BAN-CC3-BAN-VAR AND
                        CC4-BAN-VAR = ECO9BAN-CC4-BAN-VAR)AND
                       (COD-BAN-LISTA <>
                        ECO9BAN-COD-BAN-LISTA(ECO9BAN-ITEM-LISTA)) THEN
                        MOVE "S"  TO CCC-REPETIDA
                        MOVE "NO" TO ECO9BAN-SALIDA
                    END-IF
                 END-IF
              END-IF
            END-PERFORM
         ELSE
           IF FCD-ERROR <> "23" THEN
              MOVE "N" TO OP-EXITO
              PERFORM PRESENTA-ERROR
           END-IF
         END-IF

         IF CUENTA-REPETIDA = "S" THEN
            PERFORM MENSAJE-CUENTA-REPETIDA
            MOVE 4    TO ECO9BAN-CAMPO-ERROR
         ELSE
            IF  CCC-REPETIDA = "S" THEN
               PERFORM MENSAJE-CCC-REPETIDA
               MOVE 3    TO ECO9BAN-CAMPO-ERROR
            END-IF
         END-IF

       .

       MENSAJE-CUENTA-REPETIDA.
         MOVE "Atención" to stawmsg-titulo
         MOVE "No se puede repetir la cuenta."
              TO stawmsg-texto
         move stawmsg-ok  to stawmsg-boton
         move stawmsg-information  to stawmsg-icono
         call "STAWMSG" using np-stawmsg stawmsg-lkg
         cancel "STAWMSG"
       .

       MENSAJE-CCC-REPETIDA.
         MOVE "Atención" to stawmsg-titulo
         MOVE "No se puede repetir el C.C.C."
              TO stawmsg-texto
         move stawmsg-ok  to stawmsg-boton
         move stawmsg-information  to stawmsg-icono
         call "STAWMSG" using np-stawmsg stawmsg-lkg
         cancel "STAWMSG"
       .

       Comprobar-cuenta-repetida-2.
         INITIALIZE CUENTA-REPETIDA-2
         INITIALIZE CCC-REPETIDA-2
         MOVE "N" TO CUENTA-REPETIDA-2
         MOVE "N" TO CCC-REPETIDA-2

         INITIALIZE COD-ACT-VAR-TEMPOR
         MOVE COD-ACT-VAR TO COD-ACT-VAR-TEMPOR
         move cta-con-var to CTA-CON-VAR-TMP
         move CC1-BAN-VAR to CC1-BAN-VAR-TMP
         move CC2-BAN-VAR to CC2-BAN-VAR-TMP
         move CC3-BAN-VAR to CC3-BAN-VAR-TMP
         move CC4-BAN-VAR to CC4-BAN-VAR-TMP

         move reg-feeeevar to reg-feeeevar-aux

         INITIALIZE REG-FEEEEVAR
         MOVE "BAN"           TO COD-REG-VAR
         MOVE 1               TO CLAVE-VAR
         MOVE SPACES          TO COD-ACT-VAR
         CALL "ST-VAR" USING REG-FEEEEVAR, ">=", CLAVE-VAR, FCD-ERROR
         IF ERROR-1 = "0" OR FCD-ERROR = "9D" THEN
            PERFORM UNTIL (ERROR-1 <> "0" AND FCD-ERROR <> "9D") OR
                          CONTADOR>=Max-Bancos OR
                          COD-REG-VAR <> "BAN"
              CALL "RN-VAR" USING "N", FCD-ERROR
              IF (ERROR-1 = "0" OR FCD-ERROR = "9D") AND
                 COD-REG-VAR = "BAN" THEN
      *          move ccc-ban-var to ccc-ban-var-tmp2
                 move CC1-BAN-VAR to CC1-BAN-VAR-tmp2
                 move CC2-BAN-VAR to CC2-BAN-VAR-tmp2
                 move CC3-BAN-VAR to CC3-BAN-VAR-tmp2
                 move CC4-BAN-VAR to CC4-BAN-VAR-tmp2
                 add 1 to contador-2
                 perform mensaje-progreso
                 move "N" to cuenta-repetida-2
                 move "N" to ccc-repetida-2
                 MOVE COD-ACT-VAR TO COD-BAN-LISTA
                 IF ECO9BAN-MODO = "CONS" THEN
                    IF (CTA-CON-VAR = CTA-CON-VAR-TMP)
                       AND (COD-BAN-LISTA <> COD-ACT-VAR-TEMPOR)
                       THEN
                       MOVE "S"  TO CUENTA-REPETIDA-2
                       MOVE "NO" TO ECO9BAN-SALIDA
                    END-IF
                    IF (CC1-BAN-VAR = CC1-BAN-VAR-TMP AND
                        CC2-BAN-VAR = CC2-BAN-VAR-TMP AND
                        CC3-BAN-VAR = CC3-BAN-VAR-TMP AND
                        CC4-BAN-VAR = CC4-BAN-VAR-TMP)AND
                        (COD-BAN-LISTA <> COD-ACT-VAR-TEMPOR)
                        THEN
                        MOVE "S"  TO CCC-REPETIDA-2
                        MOVE "NO" TO ECO9BAN-SALIDA
                    END-IF
                    if cuenta-repetida-2 = "S" and
                       ccc-repetida-2 = "S"
                       delete feeeevar
                    end-if
                 END-IF
              END-IF
            END-PERFORM
         ELSE
           IF FCD-ERROR <> "23" THEN
              MOVE "N" TO OP-EXITO
              PERFORM PRESENTA-ERROR
           END-IF
         END-IF

         *> nos reposicionamos en el registro
         move reg-feeeevar-aux to reg-feeeevar
         move 1 to clave-var
         call "st-var" using reg-feeeevar
                             ">="
                             clave-var
                             fcd-error
         evaluate fcd-error
         when "00"
         when "23"
             call "RN-VAR" using "N", fcd-error
         when other
             perform presenta-error
         end-evaluate


       .

      ******************************************************************
      *            Tratamiento de la entrada rapida de cta's           *
      ******************************************************************

       CUENTA-NUM-A-CUENTA-ALF.
      ******************************************************************
          MOVE SPACES TO ECO9BAN-CTA-CON-VAR-TXT

          IF ECO9BAN-CTA-CON-VAR <> 0 THEN
             MOVE ECO9BAN-CTA-CON-VAR(1:ECO-DIGITOS)
               TO ECO9BAN-CTA-CON-VAR-TXT(1:ECO-DIGITOS)
          END-IF
       .

       CUENTA-ALF-A-CUENTA-NUM.
      ******************************************************************
          MOVE ZEROS TO ECO9BAN-CTA-CON-VAR

          IF ECO9BAN-CTA-CON-VAR-TXT <> SPACES THEN
             MOVE ECO9BAN-CTA-CON-VAR-TXT(1:ECO-DIGITOS)
               TO ECO9BAN-CTA-CON-VAR(1:ECO-DIGITOS)
          END-IF
       .

       EXPANDIR-CUENTA.
      ******************************************************************
          MOVE ECO9BAN-CTA-CON-VAR-TXT TO CUENTA-FTCC
          PERFORM EXPANDIR-COMPTE
          MOVE CUENTA-FTCC TO ECO9BAN-CTA-CON-VAR-TXT
          PERFORM CUENTA-ALF-A-CUENTA-NUM
       .

       ACTIVAR-COMPTES-REDUITS.
      ******************************************************************
          MOVE ECO-DIGITOS TO DIGITOS-MASCARA
          CALL WINAPI "A3FTCC" USING BY VALUE ECO9BAN-HANDLE-CTA
                                     BY REFERENCE DIGITOS-MASCARA
       .

       EXPANDIR-COMPTE.
      ******************************************************************
          MOVE ECO-DIGITOS TO DIGITOS-CUENTA-FTCC
          CALL WINAPI "A3ExpandirCuenta" USING
                                         BY REFERENCE CUENTA-FTCC
                                         BY VALUE DIGITOS-CUENTA-FTCC
       .

       PASAR-DATOS.
      ******************************************************************
          MOVE CC1-BAN-VAR-LKG(ECO9BAN-ITEM-LISTA) TO CC1-BAN-VAR-LKG(1)
          MOVE CC2-BAN-VAR-LKG(ECO9BAN-ITEM-LISTA) TO CC2-BAN-VAR-LKG(1)
          MOVE CC3-BAN-VAR-LKG(ECO9BAN-ITEM-LISTA) TO CC3-BAN-VAR-LKG(1)
          MOVE CC4-BAN-VAR-LKG(ECO9BAN-ITEM-LISTA) TO CC4-BAN-VAR-LKG(1)

          move "S" to fin-programa
          move "CR" to Salida

       .
       Calcular-Iban.
      ******************************************************************
           initialize EstCIBan
           move ECO9BAN-CC1-BAN-VAR    to CIBan-CCC-Bco
           move ECO9BAN-CC2-BAN-VAR    to CIBan-CCC-Ofi
           move ECO9BAN-CC3-BAN-VAR    to CIBan-CCC-Dc
           move ECO9BAN-CC4-BAN-VAR    to CIBan-CCC-Cta
           call var-CCCtoIBAN using EstCIban
           move CIban-Cod to eco9ban-iban-txt
       .

       VerAser-CCC.
           *> Comprobar si A3Asesor está preparado para que los campos
           *> CCC sean de display
           initialize VerCCC
           initialize ecoasope
           set EcoAsOpe-Ope-VerCCC to true
           call "EcoAsOpe" using EcoAsope
           cancel "EcoAsOpe"
           if EcoAsOpe-Sts-Ok
               move "S" to VerCCC
           end-if
       .

       Mensaje-progreso.
           initialize stp-msg(1)
           move "Bancos de la Empresa" to stp-titol

           move "Empresa" to stp-negreta(1)
           string eco-codigo-empresa delimited size
                  " - "              delimited size
                  eco-nombre-empresa delimited size
                  into stp-msg(1)

           initialize stp-msg (2)
           string "Eliminando Cuenta banco " delimited size
            " - " contador-2 delimited size
             into stp-msg (2)

           initialize stp-msg (3)
           string  CC1-BAN-VAR-TMP2 delimited size
                   " - "
                   CC2-BAN-VAR-TMP2 delimited size
                   " - "
                   CC3-BAN-VAR-TMP2 delimited size
                   " - "
                   CC4-BAN-VAR-TMP2 delimited size
           into stp-msg (3)

           perform moduloprogreso

       .

       inicializarprogreso.
           initialize stp-staprog
           move 5 to np-staprog

           perform moduloprogreso


       .

       finalizarprogreso.
           move all x"00" to stp-staprog
           call "staprog" using np-staprog,
                                stp-staprog
           cancel "staprog"
       .

       moduloprogreso.
           call "staprog" using np-staprog,
                                stp-staprog


       .

