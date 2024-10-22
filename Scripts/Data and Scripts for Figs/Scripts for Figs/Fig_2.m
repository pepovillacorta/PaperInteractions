
%% %%%%%%%%%%%%%%%%%%%%%%%%   PANELS A-C  %%%%%%%%%%%%%%%%%%%%%%%%

clear all
clc

% %% Leemos datos

% ¡ATENCIÓN! En los .csv NO hay que tener en cuenta el 'std_error'; SÓLO
% los 'conf.low' y 'conf_high', que son los intervalos del std_error PERO
% TRANSFORMADOS debio a la regresión logística.

% --------- Data for Panel A (First Draft) -------------

datos_Fig_4A = readtable('fig4_panel_A.csv');

medias_Learn_A = flipud(datos_Fig_4A.predicted);
error_low_Learn_A = flipud(datos_Fig_4A.conf_low);
error_high_Learn_A = flipud(datos_Fig_4A.conf_high);

% -------  Data for Panel B (First Draft - histograms) ----------

datos_Fig_4B_fit = readmatrix('fig4_panel_B_fit.csv', 'Delimiter',';', 'DecimalSeparator',',');
datos_Fig_4B_fit = datos_Fig_4B_fit(2:end, 2:6);
sesion_fit_B = datos_Fig_4B_fit(1:2:end, 1);
medias_fit_NR_B = datos_Fig_4B_fit(1:2:end, 2);
medias_fit_R_B = datos_Fig_4B_fit(2:2:end, 2);

datos_Fig_4B_histogram = readmatrix('fig4_panel_B_histogram.csv', 'Delimiter',';', 'DecimalSeparator',',');
sesion_hist_B = datos_Fig_4B_histogram(2:76, 2);
medias_hist_NR_B = datos_Fig_4B_histogram(2:76, 5);
medias_hist_R_B = datos_Fig_4B_histogram(77:end, 5);

% ------- Data for Panel C (First Draft - survival curves) -------

datos_Fig_4C = readmatrix('fig4_panel_C.csv', 'Delimiter',';', 'DecimalSeparator',',');
sesion_surv_C = datos_Fig_4C(2:2:end, 2);
media_surv_NR_C = datos_Fig_4C(2:2:end, 3);
media_surv_R_C = datos_Fig_4C(3:2:end, 3);
conf_low_NR_C = datos_Fig_4C(2:2:end, 4);
conf_low_R_C = datos_Fig_4C(3:2:end, 4);
conf_high_NR_C = datos_Fig_4C(2:2:end, 5);
conf_high_R_C = datos_Fig_4C(3:2:end, 5);

%% Ploteamos
clc

color=[[255, 133, 0];[0, 0, 145]]/255;

font_size_axis = 16;
font_size_labels = 20;
font_size_asterix = 40;

altura_entre_paneles = 0.1;
ancho_entre_paneles = 0.1;

dist_hasta_Abajo = 0.17;
dist_hasta_Arriba = 0.07;

dist_hasta_Izquierda = 0.06;
dist_hasta_Derecha = 0.02;

subplot = @(m,n,p) subtightplot (m, n, p, [altura_entre_paneles ancho_entre_paneles], [dist_hasta_Abajo dist_hasta_Arriba], [dist_hasta_Izquierda dist_hasta_Derecha]);

figure('color', 'w', 'position', [50, 50, 900, 300]);

subplot(2, 15, [1:4, 16:19]);
hold on;
for i = 1:2
    errorbar(i, medias_Learn_A(i), medias_Learn_A(i)-error_low_Learn_A(i), error_high_Learn_A(i)-medias_Learn_A(i), 'o','CapSize',13, 'LineWidth', 2.5, 'MarkerSize', 8, 'Color', color(i,:), 'MarkerEdgeColor', color(i,:), 'MarkerFaceColor', color(i,:))
end
set(gca, 'linewidth', 1, 'fontsize', font_size_axis)
xlh = xlabel('stimulus type', 'fontsize', font_size_labels);
xlh.Position(1) = xlh.Position(1);
xlh.Position(2) = xlh.Position(2)+0.06;
ylh = ylabel('lever-pressing prob', 'fontsize', font_size_labels);
ylh.Position(1) = ylh.Position(1)-0.72;
ylh.Position(2) = ylh.Position(2)+0.002;
xticks([])
xlim([0.5, 2.5]);
ylim([0.05, 0.6]);
text(0.45,0.9,'*','Units','normalized', 'fontsize', font_size_asterix, 'color', color(1,:))

% Histograms
subplot(2, 15, [5:10]);
hold on;
histogram('BinEdges',sesion_hist_B','BinCounts',medias_hist_R_B(1:length(sesion_hist_B)-1)', 'facecolor', color(1, :), 'edgecolor', 'w');
plot(sesion_fit_B, medias_fit_R_B, 'color', color(1, :), 'LineWidth',3);
xlim([0, 15]);
ylim([0, 1.7e-4]);
set(gca, 'linewidth', 1, 'fontsize', font_size_axis)
ylh1 = ylabel('lever-pressing prob', 'fontsize', font_size_labels);
ylh1.Position(1) = ylh1.Position(1) - 0.3;
ylh1.Position(2) = ylh1.Position(2)-0.000127;
text(0.45,0.85,'*','Units','normalized', 'fontsize', font_size_asterix, 'color', color(1,:))
xticks([]);

subplot(2, 15, [20:25]);
hold on;
histogram('BinEdges',sesion_hist_B','BinCounts',medias_hist_NR_B(1:length(sesion_hist_B)-1)', 'facecolor', color(2, :), 'edgecolor', 'w');
plot(sesion_fit_B, medias_fit_NR_B, 'color', color(2, :), 'LineWidth',3);
xlim([0, 15]);
ylim([0, 1.7e-4]);
set(gca, 'linewidth', 1, 'fontsize', font_size_axis)
xlabel('time from stimulus onset (s)', 'fontsize', font_size_labels);

% survival curves
subplot(2, 15, [11:15, 26:30]);
hold on
fill([sesion_surv_C; flipud(sesion_surv_C)], [conf_low_R_C; flipud(conf_high_R_C)], color(1, :), 'EdgeColor', 'none','FaceAlpha',.25)
fill([sesion_surv_C; flipud(sesion_surv_C)], [conf_low_NR_C; flipud(conf_high_NR_C)], color(2, :), 'EdgeColor', 'none','FaceAlpha',.25)
p1 = plot(sesion_surv_C, media_surv_R_C, 'LineWidth', 2, 'Color', color(1,:));
p2 = plot(sesion_surv_C, media_surv_NR_C, 'LineWidth', 2, 'Color', color(2,:));
set(gca, 'linewidth', 1, 'fontsize', font_size_axis)
xlabel('time from stimulus onset (s)', 'fontsize', font_size_labels);
ylh = ylabel('1st lever-pressing prob', 'fontsize', font_size_labels);
ylh.Position(1) = ylh.Position(1)-0.3;
ylh.Position(2) = ylh.Position(2);
text(0.1,0.9,'*','Units','normalized', 'fontsize', font_size_asterix, 'color', color(1,:))



%% %%%%%%%%%%%%%%%%%%%%%%%%   PANELS D-F  %%%%%%%%%%%%%%%%%%%%%%%%

clear all
clc

% Leemos datos

% ¡ATENCIÓN! En los .csv NO hay que tener en cuenta el 'std_error'; SÓLO
% los 'conf.low' y 'conf_high', que son los intervalos del std_error PERO
% TRANSFORMADOS debio a la regresión logística.

% --------- Data for Panel D (First Draft) -------------

datos_Fig_4D = readtable('fig4_panel_D.csv');

medias_Learn_D = flipud(datos_Fig_4D.predicted);
error_low_Learn_D = flipud(datos_Fig_4D.conf_low);
error_high_Learn_D = flipud(datos_Fig_4D.conf_high);

% -------  Data for Panel E (First Draft - histograms) ----------

datos_Fig_4E_fit = readmatrix('fig4_panel_E_fit.csv', 'Delimiter',';', 'DecimalSeparator',',');
datos_Fig_4E_fit = datos_Fig_4E_fit(2:end, 2:6);
sesion_fit_E = datos_Fig_4E_fit(1:2:end, 1);
medias_fit_NR_E = datos_Fig_4E_fit(1:2:end, 2);
medias_fit_R_E = datos_Fig_4E_fit(2:2:end, 2);

datos_Fig_4E_histogram = readmatrix('fig4_panel_E_histogram.csv', 'Delimiter',';', 'DecimalSeparator',',');
sesion_hist_E = datos_Fig_4E_histogram(2:51, 2);
medias_hist_NR_E = datos_Fig_4E_histogram(2:51, 5);
medias_hist_R_E = datos_Fig_4E_histogram(52:end, 5);

% ------- Data for Panel F (First Draft - survival curves) -------

datos_Fig_4F = readmatrix('fig4_panel_F.csv', 'Delimiter',';', 'DecimalSeparator',',');
sesion_surv_F = datos_Fig_4F(2:2:end, 2);
media_surv_NR_F = datos_Fig_4F(2:2:end, 3);
media_surv_R_F = datos_Fig_4F(3:2:end, 3);
conf_low_NR_F = datos_Fig_4F(2:2:end, 4);
conf_low_R_F = datos_Fig_4F(3:2:end, 4);
conf_high_NR_F = datos_Fig_4F(2:2:end, 5);
conf_high_R_F = datos_Fig_4F(3:2:end, 5);

%% Ploteamos
clc

color=[[176, 0, 249];[0, 102, 0]]/255;

font_size_axis = 16;
font_size_labels = 20;
font_size_asterix = 40;

altura_entre_paneles = 0.1;
ancho_entre_paneles = 0.1;

dist_hasta_Abajo = 0.17;
dist_hasta_Arriba = 0.07;

dist_hasta_Izquierda = 0.06;
dist_hasta_Derecha = 0.02;

subplot = @(m,n,p) subtightplot (m, n, p, [altura_entre_paneles ancho_entre_paneles], [dist_hasta_Abajo dist_hasta_Arriba], [dist_hasta_Izquierda dist_hasta_Derecha]);

figure('color', 'w', 'position', [50, 50, 900, 300]);

subplot(2, 15, [1:4, 16:19]);
hold on;
for i = 1:2
    errorbar(i, medias_Learn_D(i), medias_Learn_D(i)-error_low_Learn_D(i), error_high_Learn_D(i)-medias_Learn_D(i), 'o','CapSize',13, 'LineWidth', 2.5, 'MarkerSize', 8, 'Color', color(i,:), 'MarkerEdgeColor', color(i,:), 'MarkerFaceColor', color(i,:))
end
set(gca, 'linewidth', 1, 'fontsize', font_size_axis)
xlh = xlabel('stimulus type', 'fontsize', font_size_labels);
xlh.Position(1) = xlh.Position(1);
xlh.Position(2) = xlh.Position(2)-0.39;
ylh = ylabel('lever-pressing prob', 'fontsize', font_size_labels);
ylh.Position(1) = ylh.Position(1)-0.662;
ylh.Position(2) = ylh.Position(2)-0.178;
xticks([])
xlim([0.5, 2.5]);
ylim([0.05, 0.6]);
text(0.45,0.9,'*','Units','normalized', 'fontsize', font_size_asterix, 'color', color(1,:))

% Histograms
subplot(2, 15, [5:10]);
hold on;
histogram('BinEdges',sesion_hist_E','BinCounts',medias_hist_R_E(1:length(sesion_hist_E)-1)', 'facecolor', color(1, :), 'edgecolor', 'w');
plot(sesion_fit_E, medias_fit_R_E, 'color', color(1, :), 'LineWidth',3);
xlim([0, 10]);
ylim([0, 1.7e-4]);
set(gca, 'linewidth', 1, 'fontsize', font_size_axis)
ylh1 = ylabel('lever-pressing prob', 'fontsize', font_size_labels);
ylh1.Position(1) = ylh1.Position(1) - 0.2;
ylh1.Position(2) = ylh1.Position(2)-0.000127;
text(0.45,0.9,'*','Units','normalized', 'fontsize', font_size_asterix, 'color', color(1,:))
xticks([])


subplot(2, 15, [20:25]);
hold on;
histogram('BinEdges',sesion_hist_E','BinCounts',medias_hist_NR_E(1:length(sesion_hist_E)-1)', 'facecolor', color(2, :), 'edgecolor', 'w');
plot(sesion_fit_E, medias_fit_NR_E, 'color', color(2, :), 'LineWidth',3);
xlim([0, 10]);
ylim([0, 1.7e-4]);
set(gca, 'linewidth', 1, 'fontsize', font_size_axis)
xlabel('time from stimulus onset (s)', 'fontsize', font_size_labels);

% survival curves
subplot(2, 15, [11:15, 26:30]);
hold on
fill([sesion_surv_F; flipud(sesion_surv_F)], [conf_low_R_F; flipud(conf_high_R_F)], color(1, :), 'EdgeColor', 'none','FaceAlpha',.25)
fill([sesion_surv_F; flipud(sesion_surv_F)], [conf_low_NR_F; flipud(conf_high_NR_F)], color(2, :), 'EdgeColor', 'none','FaceAlpha',.25)
p1 = plot(sesion_surv_F, media_surv_R_F, 'LineWidth', 2, 'Color', color(1,:));
p2 = plot(sesion_surv_F, media_surv_NR_F, 'LineWidth', 2, 'Color', color(2,:));
set(gca, 'linewidth', 1, 'fontsize', font_size_axis)
xlabel('time from stimulus onset (s)', 'fontsize', font_size_labels);
ylh = ylabel('1st lever-pressing prob', 'fontsize', font_size_labels);
ylh.Position(1) = ylh.Position(1)+ 0.5;
ylh.Position(2) = ylh.Position(2);
xlim([0, 10]);
%%













