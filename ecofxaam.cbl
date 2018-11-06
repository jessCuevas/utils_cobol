       *>**************************************************************
       *> Aplicación  : A3ECO.
       *> Programa    : VER791.CBL
       *> Descripción : Conversión a v7.91.
       *> Autor       : Jordi March.
       *>**************************************************************

       program-id. ECOFXAAM.

       special-names.
           call-convention 66 is winapi
           decimal-point is comma.

       copy "tecodir.sel".
       copy "tecoejaa.sel".
       copy "feeeeada.sel".
       copy "feeeevar.sel".
       copy "feeeeaac.sel".

       copy "feeeeaam.sel".
       copy "FEEEEaam.sel" replacing trailing ==AAM== by ==aan==
                                     trailing ==APUNTE== by ==ApuntB==
                                     leading ==Clave-AMORT== by
                                             ==Clave-AmorB==.

       select incidencias
           assign to dynamic path-incidencias
           organization line sequential
           access mode sequential
           file status fcd-error.

       copy "tecodir.fd".
       copy "tecoejaa.fd".
       copy "feeeeada.fd".
       copy "feeeevar.fd".
       copy "feeeeaac.fd".

       copy "feeeeaam.fd".
       copy "FEEEEaam.FD" replacing trailing ==AAM== by ==aan==
                                    trailing ==APUNTE== by ==ApuntB==
                                    leading ==Clave-AMORT== by
                                            ==Clave-AmorB==.

       fd incidencias.
       01 reg-incidencias.
          02 linea-incidencia          pic x(250).
          02 reg-inc-pdf               redefines linea-incidencia.
            03 emp-inc-pdf             pic x(5).
            03                         pic x.
            03 tex-inc-pdf             pic x(244).

      *=================================================================
       Working-Storage section.
      *=================================================================
       copy "A3ECO.EXT".
       copy "ECOMTFI.LKG".
       copy "STAERROR.CPY".
       copy "STAWMSG.LKG".
       copy "FILEDET.LKG".
       copy "STAPROG.LKG".
       copy "ECOMINC.LKG".
       copy "ECOMVAR.CPY".
       copy "ECOMSAC.LKG".
       copy "ECOMLIQ.LKG".
       copy "ECO1001.lkg".
       copy "feeeeaam.cpy".

       *>--- Códigos y literales de versión de la aplicación ----------
       78 NUM-VERSION                  value 7,91.
       78 VERSION-ECO                  value "7.91".
       78 VERSION-CON                  value "5.91".
       78 FICHERO-CONTROL-VERSION      value "VECOC791.VER ".
       78 FICHERO-INCIDENCIAS-VERSION  value "VER791.TXT".

       *>--- Control de errores de ficheros ---------------------------
       copy "fcderror.cpy".
       77 error-4              pic 999.
       01 Path-Feeeeaan        pic x(256).

       *>--- Códigos de incidencias -----------------------------------
       77 incid                pic xx comp-5.
       01                      pic 9.
           88 hay-incidencias   value 1 false 0.
       78 ERROR-TECODIR         value 1.
       78 ERROR-POS-EJERCICIO   value 2.
       78 ERROR-POSICIONAMIENTO value 3.
       78 ERROR-ADA             value 4.
       78 ERROR-IMPRESOS        value 5.
       78 ERROR-VAR             value 6.
       78 ERROR-Resultados      value 7.
       78 ERROR-FADA            value 8.
       78 ERROR-FAAM            value 9.
       78 ERROR-NOACTIUS        value 10.

       77 fcd-error-inc        pic xx.
       77 cod-emp-inc          pic 9(5).

       *>--- Tabla de ejercicios   ------------------------------------
      *78 Num-Ejercicios       value 10.
       78 Max-Ejercicios       value 10.

       78 Ejercicio-Minimo     value 2003.
      *01 Tabla-Lit-Ejer.
      *  03 pic 9(4) value 2006.
      *  03 pic 9(4) value 2005.
      *  03 pic 9(4) value 2004.
      *  03 pic 9(4) value 2003.
      *
      *01 Tabla-ejer. *>redefines Tabla-Lit-Ejer.
      *  03 Lit-Ejer           pic 9(4) occurs Num-Ejercicios.


       01 Ejer-Contable        pic 9(4).

       01 Estado-Eje           pic x.
       01 Estado-Var           pic x.
       01 Estado-Fichero       pic x.
       *>--- Parámetros de ECOMEMP ------------------------------------
       77 np-ecomemp           pic 99 comp-5.
       77 consolidar           pic x value "N".
       77 control-lock         pic x value "N".
       77 pedir-relacion       pic x.
       77 numero-ejer          pic 99.

       *>--- Variables de trabajo -------------------------------------
       01                      pic 9.
           88 fin-bucle        value 1 false 0.
       01                      pic 9.
           88 fin-bucle-AAM    value 1 false 0.
       01                      pic 9.
           88 Ejer-Encontrado  value 1 false 0.
       01                      pic 9.
           88 hay-progreso     value 1 false 0.
       01                      pic 9.
           88 Primera-Conversion value 1 false 0.
       01                      pic 9.
           88 fin-bucle-AAC    value 1 false 0.
       01                      pic 9.
           88 tiene-registro-1 value 1 false 0.


       01 Clave                pic 9.
       01 Condicion            pic XX.

       77 codigo-empresa-aux   pic 9(5).
       77 nombre-empresa-aux   pic x(40).
       77 Camino-Acceso-aux    pic x(40).
       77 fecha-datos-aux      pic 9(6).
       77 fecha-iniejer-aux    pic 9(9).

       01 Fin-Periodo          pic 99.
       01 Tabla-Uso.
         03 Modelo-Uso         pic x(3).
         03 Estado-Uso         pic x.
         03 Tiene-Liq          pic 99 comp-5 occurs 12.
         03 Importe-Liq        pic s9(12)v99 comp-5 occurs 12.

       01 Compensar-Devolver   pic x.

       01 Ejer                 pic xx comp-5.
      *01 Indice               pic xx comp-5.
       01 Per                  pic xx comp-5.
       01 Mes                  pic 99.
       01 Mes-Real             pic 99.

       77 op-exito-final       pic x.
       77 op-exito-tfi         pic x.
       77 op-exito-emp         pic x.
       77 op-exito-liq         pic x.
       77 op-exito-1001         pic x.

       77 salida               pic xx.
       77 aplicacion           pic x(5).
       77 version              pic x(4).
       77 clave-fichero        pic 9.
       77 conversion-ok        pic x.
       01 filler               pic 9.
           88 permitir-progreso     value 1 false 0.
       77 codigo-empresa       pic 9(5).
       01 Mkdir-Op-Exito       pic x.

       01 cod-activo           pic x(8).
       01 act-emp              pic 9(2).
       01 nro-Orden            pic 9(9) COMP-5.
       77 ultimo-Orden         pic 9(4).
       01 bloqueo              pic x.
       01 iii                  pic 999.
       01 ii                   pic 99.
       01 orden-tmp            pic 9(9).

       78 max-feeeeaam         value length of reg-feeeeaam.
       77 reg-feeeeaam-aux     pic x(max-feeeeaam).

       78 max-feeeeaac         value length of reg-feeeeaac.
       77 reg-feeeeaac-aux     pic x(max-feeeeaac).

       01 moneda-Aux           pic x.
       01 Cod-Activo-Aux       pic x(8).
       01 Fecha-Inicio-Aux     pic 9(8).
       01 Val-Adqui-Aux        pic s9(12)v99.
       01 Valor-Amo-Aux        pic s9(12)v99.
       01 Inc-Rev-Aux          pic s9(12)v99.
       01 Ind-Val-Aux          pic x.
       01 Valor-Neto-Aux       pic s9(12)v99.
       01 fec-Baja             pic 9(8).
       01 indicador-leasing    pic x.

       *> variables del modulo de calculo del plan

       01 funcion-amortizacion       pic x.

       01 indicadores.
           03 liber-amor             pic x.
           03 adq-leasing            pic x.
           03 porcen-deseado         pic 9(3)v9(6).
           03 por-fiscal             pic 9(3)v9(6).
           03 tipo-amortizacion      pic x.

       01 tabla.
           03 any-tabla              pic 9999      occurs 102.
           03 amor-contable          pic s9(12)    occurs 102.
           03 amor-incremento        pic s9(12)    occurs 102.
           03 amor-fiscal            pic s9(12)    occurs 102.
           03 porcentaje             pic 9(3)v9(6) occurs 102.

       01 tabla-aux-e.
         03 tabla-aux-item-e   occurs 102.
           05 any-aux-e              pic 9999.
           05 porcentaje-aux-e       pic 9(3)v9(6).
           05 amort-contable-aux-e   pic s9(12)v99.
           05 amort-incremento-aux-e pic s9(12)v99.
           05 amort-fiscal-aux-e     pic s9(12)v99.
           05 amort-leasing-aux-e    pic s9(12)v99.

       01 valor-amortizable          pic s9(12).
       01 valor-incremento           pic s9(12).

       01 tabla-e.
          03 any-tabla-e         pic 9999        occurs 102.
          03 amor-contable-e     pic s9(12)v99   occurs 102.
          03 amor-incremento-e   pic s9(12)v99   occurs 102.
          03 amor-fiscal-e       pic s9(12)v99   occurs 102.
          03 porcentaje-e        pic 9(3)v9(6)   occurs 102.

       01 valor-amortizable-e    pic s9(12)v99.
       01 valor-incremento-e     pic s9(12)v99.

       01 importes-listados-e.
           03 periodo-e              pic s9(12)v99.
           03 acumulado-e            pic s9(12)v99.
           03 pendiente-e            pic s9(12)v99.
           03 primer-eje-e           pic 9999.
           03 total-amortizado-e     pic s9(12)v99.
       01 fecha-inicio.
           03 dia-inicio             pic 99.
           03 mes-inicio             pic 99.
           03 any-inicio             pic 9999.
       01 fecha-baja.
           03 dia-baja               pic 99.
           03 mes-baja               pic 99.
           03 any-baja               pic 9999.
       01 fecha-pantalla.
           03 mes-natural            pic 99.
           03 mes-pantalla           pic 99.
           03 any-pantalla           pic 9999.
           03 mes-fin-pantalla       pic 99.

       *> variable para calculo de un periodo
       01 meses-reparto.
           03 mes-reparto            pic 99 occurs 6.

       01 importes-listados.
           03 periodo                pic s9(12).
           03 acumulado              pic s9(12).
           03 pendiente              pic s9(12).
           03 primer-any             pic 9999.
           03 total-amortizado       pic s9(12).

       01 num-tabla                  pic 999.



      ************************** VAR: ERRORES **********************
           01 NP-STASOMBR              PIC X.
           01 IND-INIT                 PIC X.
           01 CTRL-SOMBRA              PIC X.
      **************************************************************

      *=================================================================
      *linkage section.
      *=================================================================
       77 funcion              pic x.
      *77 emp-conv             pic 9(5).
       77 op-exito             pic x.

      *=================================================================
       procedure division.


      *=================================================================
         DECLARATIVES.
           copy "TECODIR.ERF".
           copy "feeeeada.erf".
         END DECLARATIVES.

      *=================================================================
       Inicio.
           perform Operaciones-Iniciales
           perform Tratamiento
           perform Operaciones-Finales
       .

      *=================================================================
       Operaciones-Iniciales.
      *=================================================================
           move "S" to op-exito
           set hay-incidencias to false
           set permitir-progreso to true
           move "E" to funcion

           *> Inicializar la ventana de progreso
           perform Inicializar-Progreso

           *> Obtener el nombre del TECODIR actual
           initialize tabla-operaciones
           move "N" to operacion(TFI-TECODIR)
           perform Modulo-ECOMTFI

           *> Comprobar que exista el fichero directorio de empresas
           call "CBL_CHECK_FILE_EXIST" using path-tecodir, file-details
           if return-code not= 0
               *> No existe -> Se considera que es una instalación nueva
               set permitir-progreso to false

               *> Conversión de datos generales
               perform Conversion-General

               *> Cerrar la ventana de progreso
               perform Cerrar-Progreso

               *> Salir
               move "S" to op-exito
               exit program
           end-if

           *> Nos guardamos el posicionamiento de la empresa
           move eco-codigo-empresa to codigo-empresa-aux
           move eco-nombre-empresa to nombre-empresa-aux
           move Eco-Camino-Acceso-Ficheros to Camino-Acceso-Aux
           move eco-fecha-datos to fecha-datos-aux
           move eco-fecha-ini-ejer to fecha-iniejer-aux
       .

      *=================================================================
       Tratamiento.
      *=================================================================
           evaluate funcion
           when "M"
               *> Conversión masiva al inicio de la aplicación.

               *> Exigir exclusividad
               perform Mensaje-Todos-Fuera
               if conversion-ok = "S"
                   if op-exito = "S"
                       *> Conversión de datos generales
                       perform Conversion-General
                   end-if

                   if op-exito = "S"
                       *> Conversión de los datos de todas las empresas.
                       perform Convertir-Datos-Empresas
                   end-if
               end-if

           when "E"
               perform Comprobar-Ficheros-Abiertos
               *> Conversión de los datos de la empresa posicionada.
               perform Convertir-Empresa-Actual

           end-evaluate
       .

       Mensaje-Todos-Fuera.
           *>
           *> Comprobar que "to er mundo" está fuera de la aplicación.
           *>
           initialize stawmsg-lkg
           move "Atención" to stawmsg-titulo

           if eco-aplicacion-real = "ECO"
               move "A3ECO" to aplicacion
               move VERSION-ECO to version
           else
               move "A3CON" to aplicacion
               move VERSION-CON to version
           end-if

           string
               "Se va a proceder a actualizar los datos "
               "de la aplicación.\n"
               "#"
               delimited by size
               into stawmsg-texto
           end-string

           if eco-swred = "S"
               string
                   stawmsg-texto delimited by "#"
                   "\n"
                   "Asegúrese de que no hay ningún otro usuario "
                   "utilizando la aplicación\n"
                   "antes de continuar.\n"
                   "#"
                   delimited by size
                   into stawmsg-texto
               end-string
           end-if

           string
               stawmsg-texto delimited by "#"
               "\n"
               "Se recomienda que haga una copia de seguridad "
               "de sus datos\n"
               "antes de iniciar el proceso.\n"
               delimited by size
               into stawmsg-texto
           end-string

           move STAWMSG-ENTERCANCEL to stawmsg-boton
           move STAWMSG-INFORMATION to stawmsg-icono
           call "STAWMSG" using np-stawmsg, stawmsg-lkg
           cancel "STAWMSG"
           move "N" to op-exito
           if stawmsg-retorno = STAWMSG-RETURN-ENTER
               move "S" to op-exito
           end-if

           *> Por defecto, no convertir datos
           move "N" to conversion-ok

           *> Bloquear el fichero de empresas para comprobar que no
           *> haya ningún otro usuario
           if op-exito = "S"
               set fin-bucle to false
               perform with test after until fin-bucle
                   initialize tabla-operaciones
                   move "M" to operacion(TFI-RESERVADO)
                   move "B" to operacion(TFI-TECODIR)
                   perform Modulo-ECOMTFI
                   move spaces to operacion(TFI-RESERVADO)
                   if op-exito-tfi = "N"
                       *> No ha podido abrirlo en modo exclusivo ->
                       *> hay algún otro usuario utilizando la
                       *> aplicación
                       initialize stawmsg-lkg
                       move "Atención" to stawmsg-titulo
                       string
                           "Otros usuarios están utilizando "
                           aplicacion " en estos momentos.\n\n"
                           "Para poder adaptar la aplicación "
                           "es necesario que no haya\n"
                           "ningún otro usuario utilizándola."
                           delimited by size
                           into stawmsg-texto
                       end-string
                       move STAWMSG-RETRYCANCEL to stawmsg-boton
                       move STAWMSG-WARNING to stawmsg-icono
                       call "STAWMSG" using np-stawmsg, stawmsg-lkg
                       cancel "STAWMSG"
                       if stawmsg-retorno not= STAWMSG-RETURN-RETRY
                           set fin-bucle to true
                           move "N" to op-exito
                       else
                           *> Comprueba si otro terminal ha pasado
                           *> la conversión de mientras
                           call "CBL_CHECK_FILE_EXIST" using
                               FICHERO-CONTROL-VERSION,
                               file-details
                           if return-code = 0
                               *> Otro terminal ha pasado ya la
                               *> conversión.
                               initialize stawmsg-lkg
                               move "Atención" to stawmsg-titulo
                               string
                                   "La actualización de la aplicación "
                                   "ya se ha realizado desde otro "
                                   "terminal."
                                   delimited by size
                                   into stawmsg-texto
                               end-string
                               move STAWMSG-ENTER to stawmsg-boton
                               move STAWMSG-INFORMATION to stawmsg-icono
                               call "STAWMSG" using
                                   np-stawmsg,
                                   stawmsg-lkg
                               cancel "STAWMSG"
                               set fin-bucle to true
                               move "S" to op-exito
                           end-if
                       end-if
                   else
                       perform Modulo-ECOMTFI
                       set fin-bucle to true
                       move "S" to op-exito
                       move "S" to conversion-ok
                   end-if
               end-perform
           end-if
       .
      *=================================================================
      * Comprobamos que ficheros estan abiertos para luego dejarlos
      * igual.
      *=================================================================
       Comprobar-Ficheros-Abiertos.
         call Var-Estado-Fichero using Tecoejaa
                                       Estado-Eje
         call Var-Estado-Fichero using Feeeevar
                                       Estado-Var

         if Estado-Eje = "A" or Estado-Var = "C"
            initialize tabla-Operaciones
            if Estado-Eje = "A"
               move "C" to Operacion(Tfi-Tecoejaa)
            end-if
            if Estado-Var = "C"
               move "A" to Operacion(Tfi-Feeeevar)
            end-if
            move "M" to Operacion(TFI-RESERVADO)
            perform Modulo-Ecomtfi

            *> Control del error de fichero.
         end-if
       .

      *=================================================================
      * Conversiones generales que no afectan a ninguna empresa en
      * particular.
      *=================================================================
       Conversion-General.
           *> No afecta a ninguna empresa
           move 0 to cod-emp-dir
       .

      *=================================================================
      * Conversión de todas empresas.
      *=================================================================
       Convertir-Datos-Empresas.
           *> Abrir TECODIR en modo exclusivo
           initialize tabla-operaciones
           move "M" to operacion(TFI-RESERVADO)
           move "B" to operacion(TFI-TECODIR)
           move "A" to operacion(TFI-TECOEJAA)
           perform Modulo-ECOMTFI
           move spaces to operacion(TFI-RESERVADO)
           move tabla-operaciones to tabla-operaciones-aux

           if op-exito = "N"
               call "ESTADO_FICHERO" using tecodir, fcd-error
               move "N" to op-exito
               move ERROR-TECODIR to incid
               perform Grabar-Incidencia
           else
               *> Procesar todas las empresas.
               move 0 to cod-emp-dir
               move 1 to clave-fichero
               call "ST-DIR" using
                   reg-tecodir,
                   ">=",
                   clave-fichero,
                   fcd-error
               if fcd-ok
                   set fin-bucle to false
                   perform with test after until fin-bucle
                       call "RN-DIR" using reg-tecodir, "N", fcd-error
                       if fcd-ok
                           *> Convertir la empresa
                           perform Convertir-Empresa-Actual
                       else
                           if not fcd-eof
                               move "N" to op-exito
                               move ERROR-TECODIR to incid
                               perform Grabar-Incidencia
                           end-if
                           set fin-bucle to true
                       end-if
                   end-perform
               else
                   if not fcd-not-found
                       move "N" to op-exito
                       move ERROR-TECODIR to incid
                       perform Grabar-Incidencia
                   end-if
               end-if
           end-if

           *> Cerrar el TECODIR
           move tabla-operaciones-aux to tabla-operaciones
           move "C" to operacion(TFI-TECODIR)
           move "C" to operacion(TFI-TECOEJAA)
           perform Modulo-ECOMTFI

           *> Limpiar la ventana de progreso
           perform Limpiar-Progreso
       .

      *===============================================================
      * Conversión de la empresa actualmente leída.
      *===============================================================
       Convertir-Empresa-Actual.
           *> Por defecto, se convierte correctamente
           move "S" to conversion-ok
           perform Convertir-Empresa

      *    if conversion-ok = "S"
      *        perform Marcar-Empresa-Convertida
      *    end-if
       .

      *===============================================================
      * Convertir cada uno de los ejercicios a procesar.
      *===============================================================
       Convertir-Empresa.
         perform Abrir-ficheros-activos

         if conversion-ok = "S"
            perform tratamiento2
            if conversion-ok = "S"
              perform Convertir-AAM
            end-if
         end-if

         *> finalización del progreso
         perform msgprocesofinalizado
       .

      *===============================================================
      * Tratamiento para tratar los modelos
      *===============================================================
       Convertir-AAM.
         if (Act-Par-Dir = "A" or "B")
            *> Sólo las empresas de ejercicio partido son las que
            *> estan mal. Debido a que se graban mal los indicadores
            *> de empresas
            move cod-emp-dir to eco-codigo-empresa
            move cam-acc-dir to Eco-Camino-Acceso-Ficheros

      *     perform Progreso-Amortizacion2

            perform Abrir-Ficheros-Amort
      *     perform Abrir-Ficheros-Amort-Aux

            initialize Reg-Feeeeaam
            move "1"                  to tip-reg-aam
            move eco-act-cod          to act-emp-aam
            move spaces               to cod-act-aam
            move 0                    to fec-amo-aam

            move 1 to Clave
            move ">=" to Condicion
            set Fin-Bucle-AAM to false


            call "ST-AAM" using Condicion
                                Clave
                                Fcd-Error

            if not Fcd-Ok
               Set Fin-Bucle-aam to true
               if not Fcd-not-found
                  move "N" to Conversion-ok
                  move ERROR-FAAM to incid
                  perform Grabar-Incidencia
               end-if
            end-if

            perform until Fin-Bucle-AAM
               call "RN-AAM" using "N"
                                   Fcd-Error
               if Fcd-Ok
                  perform Progreso-Amortizacion2
                  if Tip-Reg-AAM = "1"
                     *> Conversión
                     move cod-act-aam to cod-activo
                     move act-emp-aam to act-emp
                     move 0 to ultimo-Orden
                     initialize orden-tmp
                     move ord-fec-aam to orden-tmp
                     if orden-tmp = 0
                       perform Convertir-Registro
                     end-if
                  else
                     set Fin-Bucle-AAM to true
                  end-if
               else
                  set Fin-Bucle-AAM to true
                  if not Fcd-Eof
                     move "N" to Conversion-ok
                     move ERROR-FAAM to incid
                     perform Grabar-Incidencia
                  end-if
               end-if
            end-perform

            perform Cerrar-Ficheros-Amort
         else
            move ERROR-NOACTIUS to incid
            perform Grabar-Incidencia
            move "N" to conversion-ok

         end-if
       .
       Convertir-Registro.
         perform until fcd-error <> "00" or
                       act-emp-aam <> act-emp or
                       cod-act-aam <> cod-activo or
                       iii >= 102

      *    call "RN-AAM" using bloqueo
      *                      fcd-error
           evaluate fcd-error
           when "00"
           when "02"
      *       move reg-feeeeaam to reg-feeeeaam-aux
              perform Moure-Camps
              call "RW-AAM" using fcd-error
              if error-1 <> "0"
                 perform Presenta-Error
              end-if
              move reg-feeeeaam to reg-feeeeaam-aux
      *    when "23"
      *       initialize registro-1-feeeeaam
      *       perform Moure-Camps
      *       call "WR-AAM" using fcd-error
      *       if error-1 <> "0"
      *          perform Presenta-Error
      *       end-if
           when other
              if error-1 <> "0"
                 perform Presenta-Error
              end-if
           end-evaluate

           call "RN-AAM" using bloqueo
                             fcd-error

         end-perform

         *>Recuperamos el último registro del activo anterior
         move reg-feeeeaam-aux to reg-feeeeaam

         call "ST-AAM" using Condicion
                             Clave
                             Fcd-Error

         call "RN-AAM" using bloqueo
                             fcd-error

       .

       Moure-camps.
         compute nro-orden = (ultimo-orden + 1) * 100000 + 100
         add 1 to ultimo-orden
         move nro-orden to ord-fec-aam
      *  move cod-act-aam to fec-apu-aam
       .

      *===============================================================
      * Tratamiento de ficheros
      *===============================================================
       Abrir-Ficheros.
         initialize Tabla-Operaciones
         move "A" to Operacion(TFI-Feeeeada)
         move "M" to Operacion(TFI-Reservado)
         perform Modulo-Ecomtfi
         if Operacion(Tfi-Reservado) <> "M"
            move "N" to Conversion-ok
            move ERROR-FADA to incid
            perform Grabar-Incidencia
         end-if
       .

       Cerrar-Ficheros.
         initialize Tabla-Operaciones
         move "C" to Operacion(TFI-Feeeeada)
         move "M" to Operacion(TFI-Reservado)
         perform Modulo-Ecomtfi
       .

       Abrir-Ficheros-Amort.
         initialize Tabla-Operaciones
         move "A" to Operacion(TFI-Feeeeaam)
         move "M" to Operacion(TFI-Reservado)
         perform Modulo-Ecomtfi
         if Operacion(Tfi-Reservado) <> "M"
            move "N" to Conversion-ok
            move ERROR-FAAM to incid
            perform Grabar-Incidencia
         end-if
       .

       Abrir-Ficheros-Amort-Aux.
         initialize Path-Feeeeaan
         string Eco-Camino-Acceso-Ficheros delimited spaces
                "V791\" delimited size
                into path-Feeeeaan
         end-string
         call "ECOMKDIR" using "C",
                               path-Feeeeaan,
                               mkdir-op-exito
         cancel "ECOMKDIR"

         string Path-Feeeeaan delimited spaces
                Eco-Codigo-Empresa "AAM.DAT" delimited size
                into Path-Feeeeaan

         open i-o Feeeeaan
         if Fcd-Error = "00"
            set Primera-Conversion to false
         else
            set Primera-Conversion to true
            close Feeeeaan
            open output Feeeeaan
         end-if
       .

       Cerrar-Ficheros-Amort.
         initialize Tabla-Operaciones
         move "C" to Operacion(TFI-Feeeeaam)
         move "M" to Operacion(TFI-Reservado)
         perform Modulo-Ecomtfi

         close Feeeeaan
       .

       Abrir-Ficheros-Activos.
         initialize Tabla-Operaciones
         move "A" to Operacion(TFI-Feeeeaac)
         move "A" to Operacion(TFI-Feeeeaam)
         move "M" to Operacion(TFI-Reservado)
         perform Modulo-Ecomtfi
         if Operacion(Tfi-Reservado) <> "M"
            move "N" to Conversion-ok
            move ERROR-NOACTIUS to incid
            perform Grabar-Incidencia
         end-if


       .

       Cerrar-Ficheros-Activos.
         initialize Tabla-Operaciones
         move "C" to Operacion(TFI-Feeeeaac)
         move "C" to Operacion(TFI-Feeeeaam)
         move "M" to Operacion(TFI-Reservado)
         perform Modulo-Ecomtfi

       .

      *===============================================================
      * Marcar como convertida la empresa actual.
      *===============================================================
       Marcar-Empresa-Convertida.
         if funcion = "M"
             perform Progreso-Marcar-Empresa
         end-if

         move NUM-VERSION to num-ver-dir
         call "RW-DIR" using reg-tecodir, fcd-error
         if not fcd-ok
             move ERROR-TECODIR to incid
             perform Grabar-Incidencia
             move "N" to conversion-ok
         end-if
       .

      *===============================================================
      * Ventana de progreso de la empresa actual para marcarla como
      * convertida.
      *===============================================================
       Progreso-Marcar-Empresa.
         perform Preparar-Limpiar-Progreso

         move "Empresa" to stp-negreta(1)
         move cod-emp-dir to codigo-empresa
         string
             codigo-empresa(1:) " - " nom-emp-dir
             delimited by size
             into stp-msg(1)
         end-string

         perform Mostrar-Progreso
       .

      *===============================================================
       Operaciones-Finales.
      *===============================================================
           move op-exito to op-exito-final

           perform cerrar-ficheros-activos
           perform Cerrar-Progreso

           perform Cerrar-Incidencias

           *> Reposicionar en la empresa en la que estaba.
           move codigo-empresa-aux to eco-codigo-empresa
           move nombre-empresa-aux to eco-nombre-empresa
           move Camino-Acceso-Aux to Eco-Camino-Acceso-Ficheros

           move fecha-datos-aux to eco-fecha-datos
           move fecha-iniejer-aux to eco-fecha-ini-ejer

           if Funcion = "E"
              *> Dejamos los ficheros como estaban
              if Estado-Eje = "A" or Estado-Var = "C"
                 initialize Tabla-Operaciones
                 if Estado-Eje = "A"
                    move "A" to Operacion(TFI-Tecoejaa)
                 end-if
                 if Estado-Var = "C"
                    move "C" to Operacion(TFI-Feeeevar)
                 end-if
                 move "M" to Operacion(TFI-Reservado)
                 perform Modulo-Ecomtfi
              end-if
           end-if

           *> fin del programa
           move op-exito-final to op-exito

           goback
       .

      *===============================================================
      * Gestión de la ventana de progreso.
      *===============================================================
       Inicializar-Progreso.
           initialize stp-staprog
           move 9 to np-staprog
           move "N" to stp-botons
           move "Actualizando la aplicación ..." to stp-titol
           set hay-progreso to false
       .

       Limpiar-Progreso.
           perform Preparar-Limpiar-Progreso
           perform Mostrar-Progreso
       .

       Preparar-Limpiar-Progreso.
           move spaces to stp-negreta(1), stp-msg(1)
           move spaces to stp-negreta(2), stp-msg(2)
           move spaces to stp-negreta(3), stp-msg(3)
           move spaces to stp-negreta(4), stp-msg(4)
           move spaces to stp-negreta(5), stp-msg(5)
       .

       Mostrar-Progreso.
           if permitir-progreso
               call "STAPROG" using np-staprog, stp-staprog
               set hay-progreso to true
           end-if
       .

       Cerrar-Progreso.
           if permitir-progreso and hay-progreso
               move all x"00" to stp-staprog
               call "STAPROG" using np-staprog,
                                    stp-staprog
               cancel "STAPROG"
               set hay-progreso to false
           end-if
       .

      *===============================================================
      * Tratamiento de ficheros.
      *===============================================================
       Modulo-ECOMTFI.
           call "ECOMTFI" using np-ecomtfi,
                                tabla-operaciones,
                                codigo-fto,
                                mes-fichero,
                                codigo-actividad
                                op-exito-tfi
       .

      ****************************************************************
      * Tratamiento de empresas.
      ****************************************************************
       Modulo-ECOMEMP.
           move "N" to consolidar, control-lock
           call "ECOMEMP" using np-ecomemp,
                                pedir-relacion,
                                consolidar,
                                numero-ejer,
                                control-lock,
                                salida,
                                op-exito-emp
           cancel "ECOMEMP"
       .

      ****************************************************************
      * Tratamiento de impresos.
      ****************************************************************
       Modulo-Ecomliq.
         move 1 to Funcion-Liq
         CALL "ECOMLIQ" USING funcion-liq,
                              tabla-situaciones,
                              opciones-impresos,
                              op-exito-liq
         CANCEL "ECOMLIQ"
       .
      ****************************************************************
      * Tratamiento del resultado de los impresos.
      ****************************************************************
       Modulo-Eco1001.
         call "ECO1001" using tabla-resumen,
                              op-exito-1001
         cancel "ECO1001"
       .
      *===============================================================
      * Gestión de incidencias.
      *===============================================================
       Grabar-Incidencia.
           move fcd-error to fcd-error-inc
           if not hay-incidencias
               move FICHERO-INCIDENCIAS-VERSION to path-incidencias
               call "CBL_CHECK_FILE_EXIST" using
                   path-incidencias,
                   file-details
               if return-code = 0
                   *>open extend incidencias
                   *>move all "*" to reg-inc-pdf
                   *>write reg-incidencias
                   *>move spaces to reg-incidencias
                   call "cbl_delete_file" using path-incidencias
                   open output incidencias
               else
                   open output incidencias
               end-if
           end-if

           initialize emp-inc-pdf
           if cod-emp-dir not= 0
               move cod-emp-dir to cod-emp-inc
               move cod-emp-inc to emp-inc-pdf
           end-if

           move fcd-error-inc to fcd-error

           evaluate incid
           when ERROR-TECODIR
               initialize tex-inc-pdf
               if error-1 = "9"
                   move Error-Binario to error-4
                   string
                       "Se ha producido el error " error-1 " / " error-4
                       " leyendo " path-tecodir
                       delimited size
                       into tex-inc-pdf
                   end-string
               else
                   string
                       "Se ha producido el error " fcd-error
                       " leyendo " path-tecodir
                       delimited size
                       into tex-inc-pdf
                   end-string
               end-if
               write reg-incidencias

           when ERROR-POSICIONAMIENTO
               initialize tex-inc-pdf
               move "No se ha podido posicionar en la empresa."
                   to tex-inc-pdf
               write reg-incidencias

           when ERROR-ADA
               initialize tex-inc-pdf
               if error-1 = "9"
                   move Error-Binario to error-4
                   string
                       "Se ha producido el error " error-1 " / " error-4
                       " leyendo " path-Feeeeada
                       delimited size
                       into tex-inc-pdf
                   end-string
               else
                   string
                       "Se ha producido el error " fcd-error
                       " leyendo " path-feeeeada
                       delimited size
                       into tex-inc-pdf
                   end-string
               end-if
               write reg-incidencias

           when ERROR-FADA
               initialize tex-Inc-Pdf
               string "No se han podido abrir el fichero: "
                      Path-Feeeeada delimited size
                      into tex-inc-pdf
               write reg-incidencias

           when ERROR-FAAM
               initialize tex-inc-pdf
               if error-1 = "9"
                   move Error-Binario to error-4
                   string
                       "Se ha producido el error " error-1 " / " error-4
                       " leyendo " path-Feeeeada
                       delimited size
                       into tex-inc-pdf
                   end-string
               else
                   string
                       "Se ha producido el error " fcd-error
                       " leyendo " path-feeeeada
                       delimited size
                       into tex-inc-pdf
                   end-string
               end-if
               write reg-incidencias

           when ERROR-IMPRESOS
               initialize tex-Inc-Pdf
               string "No se han podido obtener los impresos del "
                      "ejercicio " Ejer-Contable delimited size
                      into tex-inc-pdf
               write reg-incidencias

           when ERROR-VAR
               initialize tex-inc-pdf
               if error-1 = "9"
                   move Error-Binario to error-4
                   string
                       "Se ha producido el error " error-1 " / " error-4
                       " leyendo " path-Feeeevar
                       delimited size
                       into tex-inc-pdf
                   end-string
               else
                   string
                       "Se ha producido el error " fcd-error
                       " leyendo " path-feeeevar
                       delimited size
                       into tex-inc-pdf
                   end-string
               end-if
               write reg-incidencias

           when ERROR-Resultados
               initialize tex-Inc-Pdf
               string "No se han podido obtener el resultado de los "
                      "modelos del ejercicio "
                      Ejer-Contable delimited size
                      into tex-inc-pdf
               write reg-incidencias


           when ERROR-NOACTIUS
               initialize tex-Inc-Pdf
               string "La empresa no tiene activos o bienes de "
                      "inversión " delimited size
                      into tex-inc-pdf
               write reg-incidencias


           end-evaluate

           if fcd-error <> "00"
               set hay-incidencias to false
           else
               set hay-incidencias to true
           end-if
       .

       Cerrar-Incidencias.
           close incidencias

           if hay-incidencias
               perform Listar-Incidencias
           end-if
       .

       Listar-Incidencias.
           initialize cabecera-incidencia

           string
               "Actualización de datos de A3" eco-aplicacion-real
               delimited by size
               into titulo-listado
           end-string
           move titulo-listado to titulo-listado-c

           move "Incidencias en la actualización"
               to cabecera-listado, cabecera-listado-c

           move "Emp.  Incidencia" TO SUBCABECERA, SUBCABECERA-C

           CALL "ECOMINC" USING PATH-INCIDENCIAS
                                "X"
                                CABECERA-INCIDENCIA
                                SALIDA
                                OP-EXITO
           CANCEL "ECOMINC"

      *    delete file incidencias
       .

      *===============================================================
      * Mensaje de progreso al tratar als observaciones.
      *===============================================================
       Progreso-Empresa.
           perform Preparar-Limpiar-Progreso

           move "Empresa" to stp-negreta(1)
           move cod-emp-dir to codigo-empresa
           string
               codigo-empresa(1:) " - " nom-emp-dir
               delimited by size
               into stp-msg(1)
           end-string

           move "Ejercicio" to stp-negreta(2)
           move Ejer-Contable to Stp-Msg(2)


           move "Comprobando" to stp-negreta(3)
           move "Modelos de IVA de la Empresa " to stp-msg(3)

           perform Mostrar-Progreso
       .


       Progreso-Actualizar.
           perform Preparar-Limpiar-Progreso

           move "Empresa" to stp-negreta(1)
           move cod-emp-dir to codigo-empresa
           string
               codigo-empresa(1:) " - " nom-emp-dir
               delimited by size
               into stp-msg(1)
           end-string

           move "Ejercicio" to stp-negreta(2)
           move Ejer-Contable to Stp-Msg(2)


           move "Actualizando" to stp-negreta(3)
           move "Resultados de los modelos de IVA" to stp-msg(3)

           perform Mostrar-Progreso
       .
       Progreso-Amortizacion2.
           perform Preparar-Limpiar-Progreso

           move "Empresa" to stp-negreta(1)
           move cod-emp-dir to codigo-empresa
           string
               codigo-empresa(1:) " - " nom-emp-dir
               delimited by size
               into stp-msg(1)
           end-string

           move "Convirtiendo" to stp-negreta(2)
           move "Indicadores de Amortizaciones generadas " to stp-msg(2)

           move "Actualizando" to stp-negreta(3)
           string "Codigo Activo - " cod-act-aam delimited by size
                  into stp-msg(3)
           perform Mostrar-Progreso
       .


       Presenta-Error.
           If error-1 <> "0"
               move "N" to Op-exito
               perform Errores
               perform Cerrar-Ficheros
               perform Operaciones-Finales
           End-if
       .

       Errores.
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

       tratamiento2.

           initialize REG-FEEEEAAC
           move 1 to CLAVE
           Call "ST-AAC" using ">="
                               CLAVE
                               FCD-ERROR
           if fcd-error <> "23"
              set fin-bucle-aac to false
              perform until fin-bucle-aac
                move "N" to BLOQUEO
                call "RN-AAC" using BLOQUEO
                                    FCD-ERROR
                if fcd-error <> "10"
                   perform presenta-error
                   perform Progreso-Amortizacion
                   if tip-reg-aac = "1"
                      move mon-ult-dir to moneda-Aux
                      move cod-act-aac to Cod-Activo-Aux
                      move fec-ina-aac to Fecha-Inicio-Aux
                      move val-adq-aac to Val-Adqui-Aux
                      move Val-Amo-aac to Valor-Amo-Aux
                      move Inc-rev-aac to Inc-Rev-Aux
                      move ind-nec-aac to Ind-Val-aux
                      move val-nec-aac to Valor-neto-aux
                      move fec-ina-aac to Fecha-Inicio-Aux
                      move fec-baj-aac to fec-baja
                      perform leer-plan
                   else
                      set fin-bucle-aac to true
                   end-if
                else
                   set fin-bucle-aac to true
                end-if
              end-perform
              perform cerrar-ficheros-activos
           end-if

       .

       Progreso-Amortizacion.
           perform Preparar-Limpiar-Progreso

           move "Empresa" to stp-negreta(1)
           move cod-emp-dir to codigo-empresa
           string
               codigo-empresa(1:) " - " nom-emp-dir
               delimited by size
               into stp-msg(1)
           end-string

           move "Leyendo" to stp-negreta(2)
           move "Activos" to stp-msg(2)
           string cod-act-aac "-" des-act-aac delimited by size
                  into stp-msg(3)

           perform Mostrar-Progreso
       .

       leer-plan.
           *> leer plan
           initialize tabla
                      tabla-e
                      tabla-aux-e
           move "1"                  to tip-reg-aam
           move cod-activo-aux       to cod-act-aam
           move eco-act-cod          to act-emp-aam
           move 0                    to fec-amo-aam
           move ">="                 to condicion
           move 1                    to clave
           call "ST-AAM" using condicion
                               clave
                               fcd-error

           if fcd-Ok
           set fin-bucle-aam to false
           set tiene-registro-1 to false
             perform until fin-bucle-aam
               move "N"          to bloqueo
               call "RNM-AAM" using reg-feeeeaam-mnd
                                    moneda-aux
                                    bloqueo
                                    fcd-error
               if fcd-error = "00" and tip-reg-aam <> "1"
                  if not tiene-registro-1
                     perform CalcularPlaAmort
                     perform Grabar-tabla
                     set fin-bucle-aam to true
                  else
                     set fin-bucle-aam to true
                  end-if
               else
                  if tip-reg-aam = "1"
                     set tiene-registro-1 to true
                  end-if
                  set fin-bucle-aam to true
               end-if
             end-perform
           end-if

       .


       CalcularPlaAmort.
           initialize mes-pantalla
           initialize meses-reparto
           initialize importes-listados-e
           initialize Mes-Pantalla
           move eco-mes-i to mes-natural
           move tip-caa-aac              to tipo-amortizacion
           move por-amo-aac              to porcen-deseado
           move por-max-aac              to por-fiscal
           move Fecha-Inicio-Aux(1:4)    to fecha-inicio(5:4)
           move Fecha-Inicio-Aux(5:2)    to fecha-inicio(3:2)
           move Fecha-Inicio-Aux(7:2)    to fecha-inicio(1:2)
           move fec-baja                 to fecha-baja
                                            fecha-pantalla

           initialize mes-fin-pantalla
           move "C"                to funcion-amortizacion
           if tipo-Amortizacion = spaces
              move "L" to Tipo-amortizacion
           end-if

           evaluate lib-amo-aac
           when "0"
             move lib-amo-aac to liber-amor
           when "1"
             move lib-amo-aac to liber-amor
           when "2"
           when " "
             move " " to liber-amor
           when "3"
             move lib-amo-aac to liber-amor
           end-evaluate

           evaluate ind-lea-aac
           when "1"
             move "2" to adq-leasing
             move "2" to indicador-leasing

           when "2"
             move "S" to adq-leasing
             move "S" to indicador-leasing

           when other
             move "N" to adq-leasing
             move " " to indicador-leasing
           end-evaluate

           perform calculo-amort

      *    move mes-Baja to Mes-Pantalla

      *    move "A"                to funcion-amortizacion
      *    perform Calculo-Amort

           perform Cargar-Tabla

       .

       Calculo-Amort.
          perform LlegirMesSReparto
          evaluate moneda-aux
          when "P"
             move valor-amo-aux   to valor-amortizable
             move inc-rev-aux     to valor-incremento
             move any-inicio      to any-pantalla
             call "ECO8105P" using funcion-amortizacion
                                   indicadores
                                   tabla
                                   valor-amortizable
                                   valor-incremento
                                   fecha-inicio
                                   fecha-baja
                                   fecha-pantalla
                                   meses-reparto
                                   importes-listados
                                   indicador-leasing

             move acumulado           to acumulado-e
             cancel "ECO8105P"
          when "E"
             move valor-amo-aux   to valor-amortizable-e
             move inc-rev-aux     to valor-incremento-e
             move any-inicio      to any-pantalla
             call "ECO8105E" using funcion-amortizacion
                                   indicadores
                                   tabla-e
                                   valor-amortizable-e
                                   valor-incremento-e
                                   fecha-inicio
                                   fecha-baja
                                   fecha-pantalla
                                   meses-reparto
                                   importes-listados-e
                                   indicador-leasing
             cancel "ECO8105E"
          end-evaluate


       .

       Grabar-tabla.
          initialize reg-feeeeaam
          move "1"                  to tip-reg-aam
          move eco-act-cod          to act-emp-aam
          move cod-activo-aux      to cod-act-aam
          move 0                    to fec-amo-aam
          move 0                    to ultimo-orden
          move ">="                 to condicion
          move 1                    to clave
          call "ST-AAM" using condicion
                          clave
                          fcd-error
          perform varying iii from 1 by 1 until iii > 102
            if any-tabla-e (iii) > 0
              move any-tabla-e (iii) to fec-amo-aam
              call "RR-AAM" using bloqueo
                                fcd-error
              evaluate fcd-error
              when "00"
              when "02"
                 perform MoureCamps
                 call "RW-AAM" using fcd-error
                 if error-1 <> "0"
                    perform Presenta-Error
                 end-if
              when "23"
                 initialize registro-1-feeeeaam
                 perform MoureCamps
                 call "WR-AAM" using fcd-error
                 if error-1 <> "0"
                    perform Presenta-Error
                 end-if
              end-evaluate
            else
              exit perform
            end-if
          end-perform

       .


       MoureCamps.
           if amort-contable-aux-e (iii) <> amo-con-aam
              initialize pta-eur-aam
              initialize pta-con-aam
              initialize pta-eur-aam
           end-if
           move porcentaje-aux-e (iii)    to por-amo-aam
           move amort-contable-aux-e (iii) to amo-con-aam
           if amort-incremento-aux-e (iii) > 0
              continue
           else
              move 0 to amo-cor-aam
           end-if
           move amort-leasing-aux-e(iii) to amo-lea-aam
           move amort-fiscal-aux-e (iii)  to amo-fis-aam

       .


       LlegirMesSReparto.
           perform varying ii from 1 by 1 until ii > 6
              move 0 to mes-reparto (ii)
           end-perform
           move reg-feeeeaac to reg-feeeeaac-aux
           initialize reg-feeeeaac
           move "2"          to tip-reg-aac
           move spaces       to cod-act-aac
           move zeros        to nro-ord-aac
           move  1           to clave
           move ">="         to condicion

           CALL "ST-AAC" USING ">="
                               CLAVE
                               FCD-ERROR

           if error-1 = "0"
             move "N"    to bloqueo
             call "RN-AAC" using bloqueo
                                 fcd-error
             if error-1 = "0" and tip-reg-aac = "2"
                         and act-emp-aac = eco-act-cod
               perform varying ii from 1 by 1 until ii > 6
                 if mes-sre-aac (ii) <> "00"
                   move mes-sre-aac(ii) to mes-reparto(ii)
                 else
                   move 00              to mes-reparto(ii)
                 end-if
               end-perform
             end-if
           end-if

           move reg-feeeeaac-aux  to reg-feeeeaac

           call "RR-AAC" using bloqueo
                               fcd-error
       .

       Cargar-tabla.
           move 1 to num-tabla

           sort tabla-aux-item-e on ascending key any-aux-e
           perform varying iii from 1 by 1 until iii > 102
             if any-tabla-e (iii) > 0
               move any-tabla-e (iii)
                                to any-aux-e (num-tabla)
               move porcentaje-e (iii)
                                to porcentaje-aux-e (num-tabla)
               move amor-contable-e (iii)
                                to amort-contable-aux-e (num-tabla)
               move amor-fiscal-e (iii)
                                to amort-fiscal-aux-e (num-tabla)
               move amor-incremento-e (iii)
                                to amort-incremento-aux-e (num-tabla)
               add 1            to num-tabla
             end-if
           end-perform

       .


       MsgProcesoFinalizado.
           Initialize stawmsg-texto
           Move "Atención" to stawmsg-titulo
           Move stawmsg-ok to stawmsg-boton
           Move stawmsg-information to stawmsg-icono
           string "Proceso finalizado." delimited size
             into stawmsg-texto
           call "stawmsg" using np-stawmsg
                                stawmsg-lkg
           cancel "stawmsg"

       .
