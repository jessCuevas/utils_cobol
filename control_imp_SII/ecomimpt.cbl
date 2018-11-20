      *=================================================================
      *=    Programa:                                           =
      *=================================================================
      *=  Descripción:             =
      *=                           =
      *=================================================================
      *= Programador: Sergio B          =
      *==================================
      *=      Versión: 7.40             =
      *=================================================================
       copy "dslang.cpy".

       Program-id. ECOMIMPT.
       SPECIAL-NAMES.
           CALL-CONVENTION 66 IS WINAPI
           DECIMAL-POINT IS COMMA.

       class-control.
           Ecocemp     Class "ecocem"
           Ecoceje     Class "ecoceje"
           Ecocmes     Class "ecocmes"
           ECoczip     Class "ecoczip"
           ECocfil     Class "ecocfil"
           Ecocerr     Class "ecocerr"
           Ecoceig     Class "ecoceig"
           Ecoctab     Class "ecoctab"
       .

           Copy "TEXPCFG.SEL".
           Copy "Tecodir.sel".
           Copy "Tecodix.Sel".
           copy "TECOPRV.SEL" replacing TRAILING ==prv== by ==prb==
                               TRAILING ==clv2-Prv== by ==clv2-prb==.

           Select Lista-Fic
               Assign to Dynamic PATH-Lista-Fic
               Organization is Line Sequential
               Access Mode is Sequential
               File Status is FCD-ERROR.

           select incidencias2
               assign       dynamic path-incidencias
               organization line sequential
               access mode  sequential
               file status  fcd-error.

           Copy "TEXPCFG.FD".
           Copy "Tecodir.fd".
           Copy "Tecodix.Fd".

           copy "TECOPRV.FD" replacing TRAILING ==prv==  by ==prb==
                                          ==IS EXTERNAL.== BY ==.==.
                                   
           FD Lista-Fic.
           01 reg-Lista-Fic.
               03 nom-fic-Lis          pic x(20).
               03 Tip-fic-lis          pic 99.

           fd incidencias2 is external.
           01 reg-inc.
              03 tex-inc               pic x(240).

       Working-Storage section.
           COPY "DS-CNTRL.MF".
           COPY "DSSYSINF.CPY".
           COPY "A3ECO.EXT".
           copy "Staerror.cpy".
           Copy "STAPROG.LKG".
           copy "Stawmsg.lkg".
           copy "Stabpr.Cpy".
           Copy "FcdError.Cpy".
           Copy "EcocZip.Cpy".
           Copy "Ecocfil.Cpy".
           Copy "Ecocem.Cpy".
           Copy "Ecoceje.Cpy".
           Copy "Ecomtfi.lkg".
           Copy "Ecoceig.cpy".
           copy "eco793.lkg".
           copy "ecomcla.lkg".

           77 version-de-la-exportacion    pic 99v99 is external.

           78 Max-Texpcfg          Value length of Reg-Texpcfg.
           01 Reg-Texpcfg-Aux      Pic X(Max-Texpcfg).
      *===========================================================
      / Objectos
           01 ObjEcocZip           Object Reference Value Null.
           01 ObjDirExp            Object Reference Value Null.
           01 ObjDirExp-Msg        Object Reference Value Null.
           01 ObjErr               Object Reference Value Null.
           01 ObjEmpresa           Object Reference Value Null.
           01 ObjEim               Object Reference Value Null.
           01 ObjErrores-zip       Object Reference Value Null.
           01 ObjErrorTxt          Object Reference Value Null.
           01 ObjFilCfg            Object Reference Value Null.
           01 TEcoczip             Usage Tipo-Ecoczip.
           01 TEcocfil             Usage Tipo-Ecocfil.
           01 TEcocDir             Usage Tipo-Ecocfil.
           01 TEcocfil-Msg         Usage Tipo-Ecocfil.
           01 Barra-Exp            Usage Tipo-Barra-Progreso.
           01 Barra-Empresa        Usage Tipo-Barra-Progreso.
           01 TEmpresa-Exp         Usage Tipo-Ecocem.
           01 TEmpresa-Aux         Usage Tipo-Ecocem.
           01 TEjercicio-Exp       Usage Tipo-Ecoceje.
           01 TEjercicio-Aux       Usage Tipo-Ecoceje.

      *===========================================================


           01                      pic 9.
               88 Abortar          value 1 false 0.
           01                      Pic 9.
               88 Fin-Bucle        Value 1 False 0.

           01                      Pic 9.
               88 Fin-Bucle-Eje    Value 1 False 0.

           01                      pic 9.
               88 Existe           value 1 false 0.
           01                      pic 9.
               88 Error-leer       value 1 false 0.

           01 Lnk-op-Exito         Pic X Comp-5.
           77 lk-Inicializar       pic x comp-5.
           77 empresa-imp          pic x comp-5.
           77 zip-imp              pic x comp-5.

           01 Num-Lic-Empresa      Pic 9(10) Comp-5.
           01 Num-Total-Empresa    Pic 9(3).
           01 Cont                 Pic 9(3).
           01 Cont2                Pic 9(3).
           01 Cont3                Pic 9(3).
           01 Camino-Exportacion   Pic X(256).
           01 Nom-Zip              Pic X(256).
           01 Path-Origen-Imp      Pic X(256).
           01 Nom-Fichero          Pic X(256).
           01 Nom-Fichero-dest     Pic X(256).
           01 Path-Des             Pic X(256) Value spaces.
           01 Tipo-Open            Pic X Comp-5.
           01 Tipo-Fichero         Pic X Comp-5.
           01 Codigo-Empresa-Imp       Pic 9(5).
           01 Empresa-Progreso     Pic X(40).
           01 Ejercicio-Progreso   Pic 9(4).
           01 Actividad-Progreso   Pic X(40).
           01 Texto-Progreso       Pic X(40).
           01 Cod-Emp-Txt          Pic 9(5).
           01 Max-Valor            Pic 9(3).
           01 Ejercicio-Aux        Pic 9(4).

           01 Fec-Inicio.
               03 Mes-Inicio       pic 9(2).
               03 Any-Inicio       pic 9(4).

           01 Fec-Fin.
               03 Mes-Fin          pic 9(2).
               03 Any-Fin          pic 9(4).

           01 Tabla-Ejercicios.
               03 Ejercicio-Item   Occurs 10.
                   05 Any-Eje      Pic 9(4) Comp-X.
                   05 Mon-Eje      Pic X.
                   05 Dig-Eje      Pic 99.
                   05 Sii-Eje      pic x.
           01 Numero-Ejercicio     Pic 9(2).
           01 lNbFiles             Pic X(4) Comp-5.
           01 lCompressedSize      Pic X(4) Comp-5.
           01 lUncompressedSize    Pic X(4) Comp-5.
           01 nCompressionRatio    Pic X(2) Comp-5.
           01 bSpanned             Pic X(3) Comp-5.
           01 Niv-Apl-Origen       Pic X.
           01 Formato-Exp          Pic X.
           01 Num-Serie-EXP        pic 9(12) comp-x.
           01 Cod-Lic-Exp          pic 9(10) comp-x.
           01 tipo-licencia        pic x.
           01 Datos-Imp            Tipo-Ecoceig.
           01 Tipo-Actualizar      Tipo-Parametros-Eig.
           01 Nif-Empresa          pic x(14).
           01 Mon-Ult              pic x.
           01 tip-emp              pic x.
           01 fecha-act-asesor     pic 9(8) comp-5.
           01 texto                pic x(240).
           01 incid-num            pic z(8)9.

           78 Fichero-Empresa      Value 0.
           78 Fichero-Tabla        Value 1.
           78 Fichero-Entorno      Value 2.
           78 Fichero-Temporal     Value 3.
           01 salida-cla           pic x(2).
           01 op-exito-cla         pic x.
           01 lnk-size             pic x(4) comp-5.
           01 num-elem             pic x(4) comp-5.
           01 size-cad             pic x(4) comp-5.
           01 Path-Descomp         pic x(256).
           01 Cod-emp-aux          pic 9(5).
           77 estado-fichero       pic x.
           77 clave-dir            pic 9.
           77 num-empresas         pic x(4) comp-5.
           01 op-exito-793         pic x.
           01 version-txt          pic z9,99.

           01 sw-viene-de-importar pic x is external.
               88 no-viene-de-importar   value "N".
               88 si-viene-de-importar   value "S".
           01 w-version            pic 99v99.
           01 w-minimo-sii         pic 99v99.
           77 codigo-ejer          pic 9(4).
       78 VERSION-sii-ECO       value 8,94.
       78 VERSION-sii-ase       value 6,94.


       77                      pic 9.
          88 hay-sii value 1, false 0.
       77 zz                   pic 99.
       01 tabla-pass is external.
          03 tabla-pass-item occurs 500.
            05 tabla-pass-zip   pic x(8).
            05 tabla-pass-pass  pic x(50).
       01 kk                pic 9(3).
       01 filler            pic x.
           88 tiene-pass    value "S" false "N".
      ************************ VAR: X"91" *****************************
       01 RESULT.
           03 F-ERROR       PIC X COMP-X.
           03 FHANDLE       PIC XX COMP-X.
           03 F-ATTROUT     PIC X COMP-X.
           03 F-TIME        PIC XX COMP-X.
           03 F-DATE        PIC XX COMP-X.
           03 F-SIZE        PIC X(4) COMP-X.
           03 F-FILEOUT     PIC X(66).

       01 FUNCTION-CODE     PIC X COMP-X.
       01 PARAMETER.
           03 F-ACTION      PIC X COMP-X.
           03 F-ATTRIN      PIC X COMP-X.
           03 F-FILEIN      PIC X(66).
       01 dir-copiar        pic x(512).
       77 of-access-mode    pic x comp-x.
       77 of-deny-mode      pic x comp-x.
       77 of-device         pic x comp-x.
       77 of-file-handle    pic x(4).
       01 Nom-Fichero-aux   Pic X(256).

       linkage section.
           Copy "Ecomimpt.Cpy".
           01 salida                   pic x(2).
           01 op-exito                 pic x.

           01 Empresa-Campo-Imp        pic 9(8) comp-x value 30.
           01 Empresa-Txt-Imp          Pic X(40).
           01 Ejercicio-Campo-Imp      pic 9(8) comp-x value 30.
           01 Ejercicio-Txt-Imp        Pic 9(4).
           01 Actividad-Campo-Imp      pic 9(8) comp-x value 30.
           01 Actividad-Txt-Imp        Pic X(40).
           01 Progreso-Campo-Imp       pic 9(8) comp-x value 30.
           01 Progreso-Txt-Imp         Pic X(40).

       procedure division using Funcion-Imp
                                Parametros-Imp
                                salida
                                op-exito.

           perform operaciones-iniciales

           If Not Abortar
               perform tratamiento
           End-If

           perform operaciones-finales
       .
      /=============================================================
       operaciones-iniciales.
           Move "CR" To Salida
           Move "S" To Op-Exito
           Set Abortar to false
           Initialize Empresa-Progreso
                      Ejercicio-Progreso
                      Actividad-Progreso
                      Texto-Progreso

           *>Descomprimimos el Zip en el directorio de descompresion
           *>pero no descomprimimos el fichero CFG
           Invoke ObjZip-Imp "GetZip" returning Nom-Zip
           Move Nom-Zip (2:5) To Codigo-Empresa-Imp
           Initialize Nom-Fichero
           String Nom-Zip (2:5)
                  Nom-Zip (8:1)
                   "CF.DAT"   Delimited Size
                   Into Nom-Fichero

           Move Path-Descompresion-imp To Path-Descomp

           *>Comprobamos si hay que tratar el Zip
           If Not Abortar
               Perform Comrpobar-Zip
           end-if

           If not abortar
               Perform Crear-Directorio
           end-if

           If not abortar
               perform mover-fichero-config
           end-if

           If Not Abortar
               *>Realizamos una copia de seguridad de la empresa
               Initialize Texto-Progreso
               String "Realizando copia de seguridad de la empresa"
                       Nom-Fichero   Delimited size
                       Into Texto-Progreso
               Perform Actualizar-Progreso
           End-If
           If Not Abortar
               Perform Descomprimir-Zip
           End-If
           If not abortar
               Call "BPpaso" using ObjBarraImp-Imp
           end-if
       .
       Crear-Directorio.
           Initialize Path-Descomp
           String Path-Descompresion-imp delimited spaces
                  nom-zip delimited "."
                  "\" delimited size
                  into Path-Descomp

           *>Comprobamos si existe el directorio sino lo creamos
           Move Path-Descomp To Nom-File-WP Of TEcocDir
           Move 78-EcocFil-Directory To Tipo-File Of TEcocDir
           Invoke EcocFil "new" Using TEcocDir
                                returning ObjDirExp

           If ObjDirExp Not = Null
               Invoke ObjDirExp "CrearDir" returning Lnk-op-Exito
               if lnk-op-exito = 0
                   Set Abortar to true
               end-if
           else
               Set Abortar to true
           End-If
       .
      *------------------------
       mover-fichero-config.
           Invoke EcocFil "newPathNomFile" Using Nom-Fichero
                                           Path-Descompresion-imp
                                returning ObjFilCfg

           If ObjFilCfg <> null
               Invoke ObjFilCfg "mover" using Path-Descomp
                                       returning lnk-op-exito
               if lnk-op-exito = 0
                   set abortar to true
               end-if
               invoke ObjFilCfg "finalize" returning ObjFilCfg
           else
               set abortar to true
           end-if

       .
      *------------------------
       Comrpobar-Zip.
           Perform Abrir-Fichero-CFG
           set Error-leer to false
           If Not Abortar
               Initialize Reg-Texpcfg
               Move "EXP" To Cod-Reg-Cfg
               Read Texpcfg
               If Fcd-Error = "00"
                   Move niv-exp-cfg To Niv-Apl-Origen
                   Move ini-exp-cfg To Fec-Inicio
                   Move fin-exp-cfg To Fec-Fin
                   Move for-exp-cfg To Formato-Exp
                   Move nsr-exp-cfg To Num-Serie-EXP
                   move cdl-exp-cfg to cod-lic-exp
                   move fat-exp-cfg to fecha-act-asesor
                   move tip-lic-cfg to tipo-licencia
                   move ver-exp-cfg to w-version
                   If Trt-Exp-Cfg <> 1
                       *> No hay que tratar el Zip porque no lo han
                       *> marcado para importar
                       Set Abortar To True
                   End-If

                   If not abortar
                   and (for-exp-cfg <> "A" and (eco-swver = "S" or "U"))
                       *>la aplicacion es asesor y la importacion
                       *>no esta es formato asesor no continuamos
                       Set Abortar To True
                   End-If

                   *> Comprobar que A3CON Asesor no pueda importar de
                   *> A3CON Asesor Plus, y viceversa
                   If not abortar
                   and for-exp-cfg = "A"
                       perform Comprobar-Asesor-Plus
                   end-if

                   If not abortar
                     if eco-version > ver-exp-cfg
                       Move "N" To Op-Exito
                       perform grabar-incidencias-version-ant
                     end-if

                     if eco-version < ver-exp-cfg
                       Move "N" To Op-Exito
                       perform grabar-incidencias-version-pos
                     end-if

                     *> Esta chapuza sirve para que en ECOCAPU se
                     *> pueda comprobar si la exportación procede de
                     *> una versión anterior a la 7.70, porque en ese
                     *> caso al eliminar los apuntes no debemos eliminar
                     *> también las observaciones, para no perderlas.
                     *> Personalmente opino que esa "solución" hace
                     *> aguas por todas partes y que se van a perder
                     *> observaciones (seguro que sí), pero me han
                     *> obligado a hacer esto.
                     *> Que Dios se apiade de mi alma.
                     if op-exito = "S"
                       move ver-exp-cfg to version-de-la-exportacion
                     end-if

                   end-if
               Else
                   Set Abortar To True
                   set Error-leer to true
               End-If
           End-IF
           Close Texpcfg
       .

       *> Comprobar que A3CON Asesor no pueda importar de
       *> A3CON Asesor Plus, y viceversa
       Comprobar-Asesor-Plus.
           evaluate true
           when eco-swver = "S" and Niv-Apl-Origen = "U"
               initialize texto
               string
                   Nom-Zip delimited by spaces
                   " - La importación procede de un A3CON Asesor Plus. "
                   "No puede importarse en A3CON Asesor."
                   into texto
               end-string
               perform grabar-incidencia

               move "N" to op-exito
               set abortar to true

           when eco-swver = "U" and Niv-Apl-Origen = "S"
               initialize texto
               string
                   Nom-Zip delimited by spaces
                   " - La importación procede de un A3CON Asesor. "
                   "No puede importarse en A3CON Asesor Plus."
                   into texto
               end-string
               perform grabar-incidencia

               move "N" to op-exito
               set abortar to true

           when eco-swver = "S" and tipo-licencia = "P"
               initialize texto
               string
                   Nom-Zip delimited by spaces
                   " - La importación corresponde a una licencia de "
                   "A3CON Asesor Plus. "
                   "No puede importarse en A3CON Asesor."
                   into texto
               end-string
               perform grabar-incidencia

               move "N" to op-exito
               set abortar to true

           when eco-swver = "U" and tipo-licencia = spaces
               initialize texto
               string
                   Nom-Zip delimited by spaces
                   " - La importación corresponde a una licencia de "
                   "A3CON Asesor. "
                   "No puede importarse en A3CON Asesor Plus."
                   into texto
               end-string
               perform grabar-incidencia

               move "N" to op-exito
               set abortar to true

           when tipo-licencia = "P"
               and (eco-swver = "A"        *> A3CON Base
                   or eco-swver = "T"      *> A3CON Base + GAC
                   or eco-swver = "L"      *> A3CON Plus
                   or eco-swver = "B"      *> A3ECO Estimaciones
                   or eco-swver = "E"      *> A3ECO Base
                   or eco-swver = "F")     *> A3ECO Base + GAC

               initialize texto
               string
                   Nom-Zip delimited by spaces
                   " - La importación corresponde a una licencia de "
                   "A3CON Asesor Plus. "
                   "No puede importarse en este nivel de aplicación."
                   delimited by size
                   into texto
               end-string
               perform grabar-incidencia

               initialize texto
               string
                   "El alta de licencias de A3CON Asesor Plus está "
                   "disponible en versiones superiores. "
                   "Consulte a su distribuidor."
                   delimited by size
                   into texto
               end-string
               perform grabar-incidencia

               move "N" to op-exito
               set abortar to true

           end-evaluate
       .

       Descomprimir-Zip.
           *> Si el fichero ZIP (nom-zip) está en la tabla de passwords
           *> Descomprimir mediante nuevo método con contraseña
           set tiene-pass to false
           perform varying kk from 1 by 1 until kk > 500
             if tabla-pass-zip(kk) = nom-zip(1:8)
               set tiene-pass to true
               exit perform
             end-if
             if tabla-pass-zip(kk) = spaces
               exit perform
             end-if
           end-perform

           if tiene-pass
             perform copiar-zip-pass
           else

           Invoke ObjZip-Imp "SetPathUnZip" Using path-descomp

           Invoke ObjZip-Imp "AddFileToExclude" Using Nom-Fichero
           Invoke ObjZip-Imp "UnZipAllFiles" returning Lnk-Op-Exito
           If Lnk-Op-Exito = 0
               Move "N" To Op-Exito
               Set Abortar To True
               *>Se han producido incidencias al descomprimir
               perform grabar-incidencias-zip
           End-If
        end-if
       .
      *------------------------------------
       Abrir-Fichero-CFG.
           Set Abortar TO False
           Initialize Path-TExpcfg
           String Path-Descomp delimited spaces
                  Nom-Fichero delimited size
                  Into Path-TExpcfg

           Close Texpcfg
           Open I-O Texpcfg
           If Fcd-Error <> "00"
               Set Abortar To True
           End-If
       .
      /=============================================================

       tratamiento.
           *>Comprobamos las licencias si esta en formato asesor
           if Formato-Exp = "A"
               perform comprobar-licencias
           end-if

           *>Hacemos el Rebuild de los ficheros del Zip
           if not abortar
               Perform Rebuild-Ficheros
           end-if

           If Not Abortar
               *>Leemos las empresas a importar
               Perform Abrir-FIchero-Cfg

               Initialize Reg-Texpcfg
               move 1 to zip-imp
               Move "EMP" To Cod-Reg-Cfg
               Start Texpcfg Key >= clv-cfg-1
               If Fcd-Error = "00"
                   Set Fin-Bucle To False
                   Perform Leer-Empresa Until Fin-Bucle
               End-If

               *> Actualiza la exportación como "Ya importada"
               Move "EXP" To Cod-Reg-Cfg
               Read Texpcfg
               If Fcd-Error = "00"
                   Move zip-imp To imp-exp-cfg
                   rewrite reg-texpcfg
               End-If

      *        move niv-exp-cfg to Niv-Apl-Origen

               Close Texpcfg
           End-If

      *    if Formato-exp ="A" and eco-swver ="S"  and not abortar
           if Formato-exp ="A" and not abortar
               perform traspasar-configuracion-apl
      *        perform modificar-licencia
           end-if
       .
      *-------------------------------------------------------------
       comprobar-licencias.
           Perform contar-empresas
           call "ESTADO_FICHERO" using Texpcfg estado-fichero
           If estado-fichero = "C"
               Perform Abrir-FIchero-Cfg
           end-if

           Initialize Reg-Texpcfg
           Move "EMP" To Cod-Reg-Cfg
           Start Texpcfg Key >= clv-cfg-1
           If Fcd-Error = "00"
               Set Fin-Bucle To False
               Perform Leer-Licencia-Empresa Until Fin-Bucle
           End-If

           If estado-fichero = "C"
               Close Texpcfg
           end-if
       .

       Leer-Licencia-Empresa.
           Read Texpcfg Next
           Evaluate Fcd-Error
           When "00"
           When "9D"
               If Cod-Reg-Cfg ="EMP"

                   if trt-emp-cfg = 1
                       perform comprobar-nif
                   end-if

                   if trt-emp-cfg = 1
                       perform Comprobar-Lic
                       If not abortar
                           If not existe
                           and num-empresas > 2
                           and (eco-swver ="S" or "U")
                               initialize texto
                               string Nom-zip delimited spaces
                                " - No está permitdo dar de alta más "
                                   "de 3 empresas en A3CON Asesor."
                                  delimited size
                               into texto
                               perform grabar-incidencia

                               set abortar to true
                               move "N" to op-exito
                               Set Fin-Bucle To True
                           end-if
                       else
                           Set Fin-Bucle To True
                       end-if
                   end-if

                   Rewrite reg-texpcfg
                   Start Texpcfg Key > clv-cfg-1
               Else
                   Set Fin-Bucle To True
               End-If
           When "10"
               Set Fin-Bucle To True
           When other
               Set Fin-Bucle To True
           End-Evaluate
       .

       Comprobar-Lic.
           Perform Comprobar-Existencia-Empresa
           if existe or Num-Serie-EXP = 0
               move "I" to funcion-eco793
           else
               if eco-swver <> "S" and eco-swver not= "U"
      *            if lic-emp-cfg = 0
      *                move "I" to funcion-eco793
      *            else
                       move "L" to funcion-eco793
      *            end-if
               else
                   move "I" to funcion-eco793
               end-if
           end-if
      *    if existe
               initialize parametros-eco793
               move Num-Serie-EXP  to cla-lic-eco793
               move cod-emp-cfg to cod-emp-eco793
                                   Cod-emp-aux
               move lic-emp-cfg to cla-emp-eco793
               Perform modulo-793
      *    end-if
       .
       Comprobar-Lic-Final.
           Perform Comprobar-Existencia-Empresa
           if existe or Num-Serie-EXP = 0
               move "I" to funcion-eco793
           else
               if eco-swver <> "S" and eco-swver not= "U"
                   if lic-emp-cfg = 0
                       move "I" to funcion-eco793
                   else
                       move "L" to funcion-eco793
                   end-if
               else
                   move "I" to funcion-eco793
               end-if
           end-if
      *    if existe
               initialize parametros-eco793
               move Num-Serie-EXP  to cla-lic-eco793
               move cod-emp-cfg to cod-emp-eco793
                                   Cod-emp-aux
               move lic-emp-cfg to cla-emp-eco793
               Perform modulo-793
      *    end-if
       .

       Comprobar-Existencia-Empresa.
           Set Existe to false
           initialize reg-tecodir
           move cod-emp-cfg to cod-emp-dir
           call "rr-dir" using reg-tecodir
                               "N"
                               fcd-error
           if fcd-error = "00" or = "9D"
               set existe to true
           end-if
       .

       comprobar-nif.
           initialize reg-tecodir
           move cod-emp-cfg to cod-emp-dir
           call "rr-dir" using reg-tecodir
                               "N"
                               fcd-error
           if fcd-error = "00" or = "9D"
               if nif-per-dir <> nif-emp-cfg

                   move 0 to trt-emp-cfg
                   Move "N" To Op-Exito

                   move cod-emp-cfg to cod-emp-aux
                   string "El NIF de la empresa " cod-emp-aux
                   " - " NOM-EMP-CFG
                      " no coincide con el de la aplicación."
                       delimited size
                          into texto
                   perform grabar-incidencia
               end-if
           end-if
       .
       modulo-793.
           move num-serie-exp to cla-lic-eco793
           move 0 to mostrar-eco793
           call "eco793" using funcion-eco793
                               parametros-eco793
                               salida
                               op-exito-793
           cancel "eco793"
           if op-exito-793 ="N" or salida <> "CR"
               if salida <> "ES"
                   Perform Grabar-Incidencia-lic
               else
                   initialize texto
                   string Nom-zip delimited spaces
                        " - Importación cancelada por el usuario."
                             delimited size
                        into texto
                   perform grabar-incidencia

               end-if
               Move "N" To Op-Exito
               set abortar to true
               move 0 to trt-emp-cfg
           else
               move cla-emp-eco793 to lic-emp-cfg
           end-if
       .

       contar-empresas.
           initialize reg-tecodir, num-empresas
           move 1 to clave-dir
           call "st-dir" using reg-tecodir
                               ">="
                               clave-dir
                               fcd-error
           evaluate fcd-error
           when "00"
               perform until error-1<>"0" and fcd-error<>"9D"
                   call "rn-dir" using reg-tecodir
                                       "N"
                                       fcd-error
                   evaluate true
                   when error-1="0" or fcd-error="9D"
                       add 1 to num-empresas
                   when fcd-error="10"
                       continue
                   when other
                       set abortar to true
                   end-evaluate
               end-perform
           when "23"
               continue
           when other
               set abortar to true
           end-evaluate
       .

      *-------------------------------------------------------------
       Leer-Empresa.
           Read Texpcfg Next
           Evaluate Fcd-Error
           When "00"
           When "9D"
               If Cod-Reg-Cfg ="EMP"
                   If trt-emp-cfg <> 0
                       *>La empresa ha sido seleccionada
                       Move Reg-Texpcfg To Reg-Texpcfg-Aux
                       If tes-emp-cfg = 1
                           move 78-Sobrescribir
                               to vencimientos-eig of tipo-actualizar
                       else
                           move 78-No-tratar
                               to vencimientos-eig of tipo-actualizar
                       end-if
                       *>>>> BUS
                       if Bus-Emp-Cfg = 1
                          move 78-Sobrescribir
                               to Mar-Bus-Eig of tipo-actualizar
                       else
                          move 78-No-Tratar
                               to Mar-Bus-Eig of tipo-actualizar
                       end-if

                       Perform Tratar-Empresa
                       Perform Comprobar-Ejercicios-Empresa
                       Move Reg-Texpcfg-Aux To Reg-Texpcfg
                       Move empresa-imp to imp-emp-cfg
                       Rewrite reg-texpcfg
                       Call "CBl_AND" Using Imp-Emp-Cfg
                                           zip-imp
                                 By Value 1 size 4

                       Move Reg-Texpcfg-Aux To Reg-Texpcfg
                       Start Texpcfg Key > clv-cfg-1
                   end-if
               Else
                   Set Fin-Bucle To True
               End-If
           When "10"
               Set Fin-Bucle To True
           When other
               Set Fin-Bucle To True
           End-Evaluate
       .
      *-------------------------------------------------------------
       Comprobar-Ejercicios-Empresa.
           *>Comprobamos si todos los ejercicios de la empresa
           *>han sido importados
           Initialize Reg-Texpcfg, Numero-Ejercicio
           Move "EJE" To Cod-Reg-Cfg
           Move Eco-Codigo-Empresa To Cod-Emp-Cfg
           Start Texpcfg Key >= clv-cfg-1
           If Fcd-Error = "00"
               move 1 to empresa-imp
               Set Fin-Bucle-Eje To False
               Perform Comprobar-Ejercicios Until Fin-Bucle-Eje
           End-If
       .
       Comprobar-Ejercicios.
           Read Texpcfg Next
           Evaluate Fcd-Error
           When "00"
           When "9D"
               If Cod-Reg-Cfg = "EJE" and
                                       Cod-Emp-Cfg = Eco-Codigo-Empresa
                   Call "CBl_AND" Using Imp-Eje-Cfg
                                       empresa-imp
                             By Value 1 size 4
               Else
                   Set Fin-Bucle-Eje To True
               End-If
           When "10"
               Set Fin-Bucle-Eje To True
           When Other
               Set Abortar To True
               Set Fin-Bucle-Eje To True
           End-Evaluate
       .
      *-------------------------------------------------------------
       Tratar-Empresa.
           *>Actualizamos el progreso
           Initialize Empresa-Progreso
                      Ejercicio-Progreso
                      Actividad-Progreso
                      Texto-Progreso
           Set si-viene-de-importar to true

           Move Cod-Emp-Cfg To Cod-Emp-Txt

           String Cod-Emp-Txt " - "
                   Nom-Emp-Cfg Delimited size
                       Into Empresa-Progreso
           Perform Actualizar-Progreso

           *>Inicalizamos la empresa
           Move Cod-Emp-Cfg To Eco-Codigo-Empresa
           Move Nif-Emp-Cfg To Nif-Empresa
           Move umo-emp-cfg To mon-ult
           Move tip-emp-cfg to tip-emp
           Move 0 To lk-Inicializar
           invoke Ecocemp "new" Using lk-Inicializar
                                returning ObjEmpresa
           *>Tratamos los ejercicios que vienen en la importación.
           Initialize Reg-Texpcfg, Numero-Ejercicio
           Move "EJE" To Cod-Reg-Cfg
           Move Eco-Codigo-Empresa To Cod-Emp-Cfg
           Start Texpcfg Key >= clv-cfg-1
           If Fcd-Error = "00"
               Set Fin-Bucle-Eje To False
               Perform Leer-Ejercicios Until Fin-Bucle-Eje
           End-If

           Move Reg-Texpcfg-Aux To Reg-Texpcfg

           If Not Abortar
               Invoke ObjEmpresa "ImportarTipica"
                                            Using Nif-Empresa
                                                  Path-Descomp
                                                  Fec-Inicio
                                                  Fec-Fin
                                                  Tabla-Ejercicios
                                                  Numero-Ejercicio
                                                  tip-emp
                                                  mon-ult
                                                  tipo-actualizar
                                                  Niv-Apl-Origen
                                                  Progreso-Imp
                                                  ObjZip-Imp
                                                  path-incidencias-impt
                                       Returning Lnk-Op-Exito

               if lnk-op-exito <> 0
                   if Formato-Exp = "A"
                       Perform Comprobar-Lic-Final
                   end-if
               end-if

               If lnk-op-exito <> 0
                   *>Marcamos los ejercicios como importados
                   Perform varying cont from 1 by 1
                                           until cont> Numero-Ejercicio

                       if Any-Eje (cont)>= Any-Inicio
                               and Any-Eje (cont)<= any-fin
                           Initialize reg-texpcfg
                           Move "EJE" To Cod-Reg-Cfg
                           Move Eco-Codigo-Empresa To Cod-Emp-Cfg
                           Move Any-Eje (cont) To
                                                       Cod-Eje-cfg
                           read texpcfg
                           if fcd-error = "00"
                               If Lnk-Op-Exito <> 0
                                   *>marcamos la empresa como importada
                                   move 1 to imp-eje-cfg
                               else
                               *>marcamos la empresa como no importada
                                   move 0 to imp-eje-cfg
                               end-if
                               rewrite reg-texpcfg
                           end-if
      *                    Rewrite reg-texpcfg
                           Move Reg-Texpcfg-Aux To Reg-Texpcfg
                       end-if
                   end-perform
               else
                   move "N" to op-exito
                   set abortar to true
               end-if
           else
               move "N" to op-exito
           End-If

           Set no-viene-de-importar to true

       .
      *------------------------------------------------------------
       Leer-Ejercicios.
           Read Texpcfg Next
           Evaluate Fcd-Error
           When "00"
           When "9D"
               If Cod-Reg-Cfg = "EJE" and
                                       Cod-Emp-Cfg = Eco-Codigo-Empresa
                                       and trt-eje-cfg = 1

                   Add 1 To Numero-Ejercicio
                   Move Cod-Eje-cfg To Any-Eje (Numero-Ejercicio)
                   Move mon-eje-cfg To Mon-Eje (Numero-Ejercicio)
                   Move dgt-eje-cfg To Dig-Eje (Numero-Ejercicio)
                   Move sii-eje-cfg To Sii-Eje (Numero-Ejercicio)
                   set hay-sii to false
                   IF sii-eje-cfg = "S"
                      set hay-sii to true
                   else
                      perform buscar-sii
                   end-if
                   if hay-sii
                      perform control-version-sii
                   end-if
               Else
                   Set Fin-Bucle-Eje To True
               End-If
           When "10"
               Set Fin-Bucle-Eje To True
           When Other
               Set Abortar To True
               Set Fin-Bucle-Eje To True
           End-Evaluate
       .
       control-version-sii.
           if formato-exp = "A"
              move version-sii-ase to w-minimo-sii
           else
              move version-sii-eco to w-minimo-sii
           end-if

           if w-version < w-minimo-sii
              perform grabar-incidencias-SII-ant
              set Abortar to True
              set Fin-bucle-Eje to true
           end-if
       .
      *------------------------------------------------------------
       Rebuild-Ficheros.
           Move "Reconstruyendo ficheros..." To Texto-Progreso
           Perform Actualizar-Progreso

           Invoke ObjZip-Imp "GetZipFileInformation" using lNbFiles
                                                       lCompressedSize
                                                   lUncompressedSize
                                                   nCompressionRatio
                                                   bSpanned
                                         REturning Lnk-Op-Exito

           Move lNbFiles To MAx-Valor
           Perform Inicializar-Barra

           Invoke ObjZip-Imp "GetZip" Returning Nom-Fichero

           Inspect Nom-Fichero replacing first ".ZIP" By ".EXP"
           Initialize path-lista-fic
           String Path-Descomp Delimited Spaces
                   Nom-fichero delimited size
                   Into path-lista-fic

           Open I-O Lista-Fic
           If Fcd-Error = "00"
               read lista-fic
               perform until fcd-error<>"00"
                   Move nom-fic-Lis To Nom-Fichero
                   Move Tip-fic-lis To Tipo-Fichero
                   Invoke Ecocfil "newPathNomFile" Using
                                                   Nom-Fichero
                                              Path-Descomp
                                       Returning ObjFile
                   Initialize Texto-Progreso
                   String "Reconstruyendo fichero "
                           Nom-Fichero   Delimited size
                           Into Texto-Progreso
                   Perform Actualizar-Progreso

                   Invoke ObjFile "RebuildDat"
                                 Using By Value Tipo-Fichero
                                           Returning Lnk-Op-Exito

                   Invoke ObjFile "finalize" returning ObjFile
                   perform Poner-paso-barra
                   read lista-fic
               end-perform
               Move Max-Valor To Valor Of ObjBarraProceso-Imp
               call "BPPonerValor" using ObjBarraProceso-Imp

           Else
               Set Abortar To True
           End-IF
           Close Lista-Fic
           Call "BPpaso" using ObjBarraImp-Imp
       .
      *------------------------------------------------------------
       traspasar-configuracion-apl.

           Move 1 To Tipo-Open

           Move Codigo-Empresa-Imp
                           To Codigo-Empresa of Datos-Imp

           Invoke Ecocerr "new" returning ObjErr-Eig of Datos-Imp

           Move Path-Descomp
                               To Path-Origen of Datos-Imp
                                  Path-Origen-Tablas of Datos-Imp
                                  Path-Origen-Ento of Datos-Imp

           Move eco-camino-acceso-tablas to path-destino-tablas
           Move eco-camino-acceso-entorno to path-destino-ento


           Move ObjBarraProceso-Imp  To Barra-Emp of Datos-Imp
           Move ObjBarraImp-Imp  To Barra-Exp of Datos-Imp
           Move ObjZip-Imp To ObjZip of Datos-Imp
           Move PProgreso-Campo-Imp To Pprogreso-Campo Of Datos-Imp
           Move PProgreso-Txt-Imp   To Pprogreso-Txt   Of Datos-Imp

           Invoke Ecoctab "new" Using  Datos-imp
                                       Tipo-Open
                               Returning ObjEim

           Invoke ObjEim "ActTAPLCONF" Using cod-lic-exp
                                             Num-Serie-EXP
                                             fecha-act-asesor
                                             Niv-Apl-Origen
                                   Returning Lnk-Op-Exito


           Invoke ObjEim "finalize" Returning ObjEim
       .
      *------------------------------------------------------------
      *modificar-licencia.
      *    initialize parametros-cla
      *    move "MF" to funcion-cla
      *    move 1 to cod-lic-cla
      *    move Num-Serie-EXP to cla-lic-cla
      *    move fecha-act-asesor to fec-lic-cla
      *    call "ecomcla" using funcion-cla
      *                         parametros-cla
      *                         op-exito-cla
      *                         salida-cla
      *    cancel "ecomcla"
      *.


      *------------------------------------------------------------
       Inicializar-Barra.
           Move 0 To rango-inicial Of ObjBarraProceso-Imp
           Move Max-Valor To rango-final Of ObjBarraProceso-Imp
           Move 1         To paso        Of ObjBarraProceso-Imp

           call "BPPonerRango" using ObjBarraProceso-Imp
           call "BPPonerPaso" using ObjBarraProceso-Imp

           Move 0 TO valor Of ObjBarraProceso-Imp
           call "BPPonerValor" using ObjBarraProceso-Imp
       .
      *------------------------------------------------------------
       Poner-paso-barra.
           Call "BPPaso" Using ObjBarraProceso-Imp
       .
      *------------------------------------------------------------
       Actualizar-Progreso.
           If PEmpresa-Campo-Imp <> Null and PEmpresa-Txt-Imp <> Null
               Set Address Of Empresa-Txt-Imp To PEmpresa-Txt-Imp
               Set Address Of Empresa-Campo-Imp To PEmpresa-Campo-Imp
               Move Empresa-Progreso To Empresa-Txt-Imp
               refresh-object Empresa-Campo-Imp
           End-If


           If PEjercicio-Campo-Imp <> Null
                                       and PEjercicio-Txt-Imp <> Null
               Set Address Of Ejercicio-Txt-Imp To PEjercicio-Txt-Imp
               Set Address Of Ejercicio-Campo-Imp
                                               To PEjercicio-Campo-Imp
               Move Ejercicio-Progreso To Ejercicio-Txt-Imp
               refresh-object Ejercicio-Campo-Imp
           End-If

           If PActividad-Campo-Imp <> Null
                                       and PActividad-Txt-Imp <> Null
               Set Address Of Actividad-Txt-Imp To PActividad-Txt-Imp
               Set Address Of Actividad-Campo-Imp
                                               To PActividad-Campo-Imp
               Move Actividad-Progreso To Actividad-Txt-Imp
               refresh-object Actividad-Campo-Imp
           End-If


           If PProgreso-Campo-Imp <> Null
                                       and PProgreso-Txt-Imp <> Null
               Set Address Of Progreso-Txt-Imp To PProgreso-Txt-Imp
               Set Address Of Progreso-Campo-Imp
                                               To PProgreso-Campo-Imp
               Move Texto-Progreso To Progreso-Txt-Imp
               refresh-object Progreso-Campo-Imp
           End-If
       .
      /=============================================================
       Grabar-Incidencia-Lic.
           initialize texto
           Evaluate incid-eco793
           when 4
                 string Nom-zip delimited spaces
                        " - No quedan más licencias diponibles."
                             delimited size
                        into texto
           when 5
               if eco-swver <> "S" and eco-swver not= "U"
                   string Nom-zip delimited spaces
                       " - La licencia seleccionada está ocupada."
                             delimited size
                        into texto
               else
                   string Nom-zip delimited spaces
                    " - No está permitdo dar de alta más "
                           "de 3 empresas en A3CON Asesor."
                          delimited size
                   into texto

               end-if
           when 6
                 string Nom-zip delimited spaces
                  " - La importación no procede de un A3CON Asesor."
                     delimited size
                        into texto
           when 7
                 string Nom-zip delimited spaces
                  " - La importación no procede "
                         "del mismo A3CON Asesor que la licencia."
                     delimited size
                        into texto
           when 8
                 string Nom-zip delimited spaces
                     " - No coinciden las licencias."
                     delimited size
                        into texto
           when 9
                 string Nom-zip delimited spaces
                     " - La empresa no está licenciada."
                     delimited size
                        into texto
           when 10
                 string Nom-zip delimited spaces
                     " - La licencia seleccionada pertenece a otro"
                        " A3CON Asesor."
                     delimited size
                        into texto
           when 12
                 string Nom-zip delimited spaces
                 " - Se ha producido un error al leer el número"
                        " de serie de A3CON Asesor."
                     delimited size
                        into texto
           when 13
                 string Nom-zip delimited spaces
                 " - No coinciden las licencias. Es posible que "
                        "la empresa " Cod-emp-aux
                        " se haya excluido de la licencia "
                     delimited size
                        into texto
               perform grabar-incidencia

               initialize texto
                 string "y se ha vuelto a dar de alta."
                 into texto
           when 14
                 string Nom-zip delimited spaces
                  " - La empresa " Cod-emp-aux
                  " pertenece una licencia "
                        "de otro A3CON Asesor."
                     delimited size
                        into texto
           when 15
                 string Nom-zip delimited spaces
                 " - La empresa " Cod-emp-aux
                 " no está licenciada. Es posible que "
                        "la empresa se haya excluido de la licencia "
                        "y esta haya sido eliminada."
                     delimited size
                        into texto
           when 16
                 string Nom-zip delimited spaces
                 " - La empresa " Cod-emp-aux
                   " no está licenciada. Es posible que "
                        "la empresa " Cod-emp-aux " se haya "
                        "excluido de la licencia."
                     delimited size
                        into texto
           when 17
                 string Nom-zip delimited spaces
                 " - No coinciden las licencias. Es posible que "
                        "la empresa " Cod-emp-aux
                        " se haya excluido de la licencia "
                        "en la aplicacion A3ECO/A3CON "
                     delimited size
                        into texto
               perform grabar-incidencia

               initialize texto
                 string "y se ha vuelto a dar de alta."
                     delimited size
                        into texto

           when 19
               string
                   nom-zip delimited by spaces
                   " - La empresa pertenece una licencia de A3CON "
                   "Asesor Plus, pero la importación procede de un "
                   "A3CON Asesor."
                   delimited by size
                   into texto
               end-string

           when 20
               string
                   nom-zip delimited by spaces
                   " - La empresa pertenece una licencia de A3CON "
                   "Asesor, pero la importación procede de un "
                   "A3CON Asesor Plus."
                   delimited by size
                   into texto
               end-string

           when 21
               string
                   nom-zip delimited by spaces
                   " - "
                   "No se pueden incluir más empresas en esta licencia."
                   delimited by size
                   into texto
               end-string

           when other
                 Move incid-eco793 to incid-num
                 string Nom-zip delimited spaces
                 " - Se ha producido el error: "
                       incid-num
                    " en la licencia."
                     delimited size
                        into texto
           end-evaluate
           perform grabar-incidencia

       .
       grabar-incidencia.
           move path-incidencias-impt to path-incidencias
           open extend incidencias2
           if error-1 = "0"
               move texto to tex-inc
               write reg-inc
           end-if
           close incidencias2
       .
       grabar-incidencias-zip.
           If ObjZip-Imp <> null
               Invoke ObjZip-Imp "getErroresZip"
                                        Returning ObjErrores-zip

               Invoke ObjErrores-zip "size" returning lnk-size

               perform varying num-elem from 1 by 1
                                               until num-elem > lnk-size

                   invoke objerrores-zip "at" using num-elem
                                           returning objerrortxt

                   invoke objerrortxt "getvalue" returning texto
                   perform grabar-incidencia

               end-perform
           end-if
       .
       grabar-incidencias-SII-ant.
           move w-version to version-txt
           initialize texto
           string "La importación " Nom-Zip(1:12) " procede de una "
      *           "aplicación desactualizada con versión "
      *           version-txt "."
                  "aplicación desactualizada en SII."
                  " Debe actualizar antes la aplicación"
                  " origen."
                  into texto

           perform grabar-incidencia
       .
       grabar-incidencias-version-ant.
           move ver-exp-cfg to version-txt
           initialize texto
           string "La importación " Nom-Zip(1:12) " procede de una "
      *           "aplicación desactualizada con versión "
      *           version-txt "."
                  "aplicación desactualizada."
                  " Es recomendable actualizar la aplicación"
                  " origen."
                  into texto

           perform grabar-incidencia
       .

       grabar-incidencias-version-pos.
           move ver-exp-cfg to version-txt
           initialize texto
           string "La importación " Nom-Zip(1:12) " procede de una "
      *           "aplicación con versión " version-txt "."
                  "aplicación más actualizada que ésta."
                  " Es recomendable actualizar esta aplicación."
                  into texto

           perform grabar-incidencia
       .
      /=============================================================
       operaciones-finales.
           If not abortar and not tiene-pass
               Perform AddFicheroCfg
           end-if
           Perform Borrar-Ficheros-Zip

           if ObjDirExp not = null
               *>Borramos el directorio
               invoke objdirexp "EliminarFile" returning lnk-op-exito
               Invoke ObjDirExp "finalize" returning objdirexp
           end-if

           If op-exito <> "N"
               Move "S" To Op-Exito
           End-if
           Exit Program
       .
      *------------------------------------------------------------
       AddFicheroCfg.
           Move path-texpcfg to nom-fichero
           Invoke ObjZip-Imp "InitFilesToProcess"
           Invoke ObjZip-Imp "InitFilesToExclude"
           Invoke ObjZip-Imp "AddFileToZip" using nom-fichero
           Invoke ObjZip-Imp "Zip" returning Lnk-Op-Exito
       .
      *------------------------------------------------------------
       Borrar-Ficheros-Zip.

           Move 1 To Tipo-Open

           Move Codigo-Empresa-Imp
                           To Codigo-Empresa of Datos-Imp

           Move Path-Descomp
                               To Path-Destino of Datos-Imp
                                  Path-Destino-Tablas of Datos-Imp
                                  Path-Destino-Ento of Datos-Imp

           Move ObjBarraProceso-Imp  To Barra-Emp of Datos-Imp
           Move ObjBarraImp-Imp  To Barra-Exp of Datos-Imp
           Move ObjZip-Imp To ObjZip of Datos-Imp


           Move PProgreso-Campo-Imp To Pprogreso-Campo Of Datos-Imp
           Move PProgreso-Txt-Imp   To Pprogreso-Txt   Of Datos-Imp

           if not Error-leer
               Invoke Ecoceig "newWithOtherProgress" Using Datos-Imp
                                                           Tipo-Open
                                                 Returning ObjEim


               if not objeim = null
                   Invoke ObjEim "BorrarFicherosLista"
                                     Returning Lnk-Op-Exito

                   Invoke ObjEim "finalize" returning ObjEim
               end-if
           end-if
       .
       buscar-sii.
          perform abrir-tecodix
          perform abrir-tecoprb

          perform leer-tecodix
          if fcd-ok
             move cod-eje-cfg to codigo-ejer
             perform leer-tecoprb
             if mes-cie-dix < 12 and not hay-sii
                add 1 cod-eje-cfg giving codigo-ejer
                perform leer-tecoprb
             end-if
          end-if


          perform cerrar-tecoprb
          perform cerrar-tecodix

       .

       ABRIR-TECODIX.
           Initialize Path-Tecodix
           String Path-DESCOMP Delimited spaces
                  "TECODIR.Dat" Delimited size
                  Into Path-Tecodix
           open input tecodix
       .
       ABRIR-TECOPRB.
           Initialize Path-Tecoprb
           String Path-DESCOMP Delimited spaces
                  "TECOPRV.Dat" Delimited size
                  Into Path-Tecoprb
           open input tecoprb
       .
       leer-tecodix.
           initialize reg-tecodix
           move COD-EMP-CFG to cod-emp-dix
           read tecodix
       .

       cerrar-tecodix.
           close tecodix
       .
       cerrar-tecoprb.
           close tecoprb
       .
       leer-tecoprb.
          initialize reg-tecoprb
          move COD-EMP-CFG to cod-emp-prb
          read tecoprb

          if fcd-ok

             if EST-ONL-EMP-PRb(2) = "OK"
                perform varying zz from 1 by 1 until zz > 10 or hay-sii
                  if eje-emp-prb(zz) = codigo-ejer
                     if EST-ONL-EJE-PRb (zz,2) = "OK"
                        set hay-sii to true
                     end-if
                  end-if
               end-perform
             end-if

          end-if
       .
       copiar-zip-pass.
           *> Copiar los ficheros de: dir-copiar
           *>                      a: path-descomp
           move nom-fichero to Nom-Fichero-aux

           initialize dir-copiar
           string eco-camino-aplicacion delimited space
                  "IMPORT\TMP\"
                  nom-zip(1:8)
                  "\"
           into dir-copiar
           perform copiar-directorio-tmp

           move Nom-Fichero-aux to Nom-Fichero
       .

       copiar-directorio-tmp.
           initialize f-filein
           string dir-copiar delimited space
                  "*.*"        delimited size
             into f-filein
           *> Iniciar la búsqueda
           INITIALIZE RESULT
           MOVE 69 TO FUNCTION-CODE
           MOVE 32 TO F-ATTRIN
           MOVE 0 TO F-ERROR

           *> Buscar ficheros
           MOVE 0 TO F-ACTION
           PERFORM with test after UNTIL F-ERROR not= 0
               move spaces to f-fileout
               CALL X"91" USING RESULT,
                                FUNCTION-CODE,
                                PARAMETER
               if F-ERROR = 0
                   EVALUATE F-ATTROUT
                       WHEN 008
                       WHEN 016
                           continue
                       WHEN OTHER
                           initialize nom-fichero
                           string  dir-copiar delimited space
                                   f-fileout    delimited size
                                   into nom-fichero
                           initialize nom-fichero-dest
                           string  path-descomp delimited space
                                   f-fileout    delimited size
                                   into nom-fichero-dest
                           *> El fichero CF no hay que copiarlo
                           if f-fileout(7:2) not= "CF"
                             PERFORM copiar-fichero
                           end-if
                   END-EVALUATE

                   MOVE 1 TO F-ACTION
               end-if
           END-PERFORM

           *> Finalizar la búsqueda
           MOVE 2 TO F-ACTION
           CALL X"91" USING RESULT,
                            FUNCTION-CODE,
                            PARAMETER
       .

       copiar-fichero.
           *> Componer el nombre completo del fichero
           call "CBL_TOUPPER" using
               nom-fichero
               by value length of nom-fichero size 4

           CALL "CBL_COPY_FILE" USING nom-fichero
                                      nom-fichero-dest
       .
       .
