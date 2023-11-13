/* Generated Code (IMPORT) */
/* Source File: grupo3.xlsx */
/* Source Path: /home/u63571459 */
/* Code generated on: 13/11/23 16:48 */

%web_drop_table(WORK.IMPORT);


FILENAME REFFILE '/home/u63571459/grupo3.xlsx';

PROC IMPORT DATAFILE=REFFILE
	DBMS=XLSX
	OUT=WORK.IMPORT;
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=WORK.IMPORT; RUN;


%web_open_table(WORK.IMPORT);


proc freq data=WORK.IMPORT;
tables Descrição_avaria / nocum nopercent out=ContagemAvarias;
run;

/* Gráfico de contagem das avarias */
proc sgplot data=ContagemAvarias;
    vbar Descrição_avaria / response=count group=Descrição_avaria
                            datalabel=count datalabelattrs=(color=black);
    title 'Contagem de Avarias';
    yaxis label='Contagem';
    xaxis label='Descrição de Avaria';
run;

/* Calcular a contagem de ocorrências para cada combinação de Prateleira e Descrição_Avaria */
proc freq data=WORK.IMPORT;
    tables Prateleira * Descrição_avaria / out=ContagemAvarias(keep=Prateleira Descrição_avaria count);
run;

/* Gráfico de avaria por prateleira */
proc sgplot data=ContagemAvarias;
    vbarparm category=Prateleira response=count / 
        group=Descrição_avaria groupdisplay=stack 
        datalabel datalabelattrs=(color=black); 

    title 'Distribuição de avaria por prateleira';
    yaxis label='Contagem' grid; 
    xaxis label='Prateleira' display=(nolabel); 

    keylegend / position=bottom title='Descrição de Avaria'; 

run;

/* Teste qui-quadrado para tipo de avaria e prateleira */
proc freq data=WORK.IMPORT;
  tables Descrição_Avaria * Prateleira / chisq;
run;

/* Coluna de contagem para cada combinação de Prateleira e Descrição_Avaria */
proc sql;
    create table SeuConjuntoComContagem as
    select *,
           count(*) as ContagemAvarias
    from WORK.IMPORT
    group by Prateleira, Descrição_Avaria;
quit;

/* Calcular a média total da variável categórica "Descrição_avaria" */
proc freq data=WORK.IMPORT;
    tables Descrição_avaria / out=TabelaFreq;
run;

/* Criar gráfico de pizza */
title 'Gráfico de Pizza das Frequências';
proc gchart data=TabelaFreq;
  pie3d Descrição_avaria / sumvar=Percent;
run;
