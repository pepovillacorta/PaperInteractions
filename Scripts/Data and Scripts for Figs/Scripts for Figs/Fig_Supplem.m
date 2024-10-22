
%% %%%%%%%%%%%%%%%%%%%%%%%%   PANELS A-C  %%%%%%%%%%%%%%%%%%%%%%%%

clear all
clc

%% Leemos datos

% ¡ATENCIÓN! En los .csv NO hay que tener en cuenta el 'std_error'; SÓLO
% los 'conf.low' y 'conf_high', que son los intervalos del std_error PERO
% TRANSFORMADOS debio a la regresión logística.

% --------- Data for Panel A (First Draft) -------------

% datos_Fig_3A = readtable('fig3_panel_A.csv');
datos_Fig_Supplm_A = readmatrix('fig2_panel_C.csv', 'Delimiter',';', 'DecimalSeparator',',');

medias_Learn_Supplm_A = datos_Fig_Supplm_A([4,5,2,3], 3);
error_low_Learn_Supplm_A = datos_Fig_Supplm_A([4,5,2,3], 5);
error_high_Learn_Supplm_A = datos_Fig_Supplm_A([4,5,2,3], 6);

% -------  Data for Panel B (First Draft - histograms) ----------

datos_Fig_Supplm_B_fit = readmatrix('fig2_panel_D_fit.csv', 'Delimiter',';', 'DecimalSeparator',',');
sesion_fit_Supplm_B = datos_Fig_Supplm_B_fit(2:4:end, 2);
medias_fit_Dyn_NR_Supplm_B = datos_Fig_Supplm_B_fit(2:4:end, 3);
medias_fit_Stat_NR_Supplm_B = datos_Fig_Supplm_B_fit(3:4:end, 3);
medias_fit_Dyn_R_Supplm_B = datos_Fig_Supplm_B_fit(4:4:end, 3);
medias_fit_Stat_R_Supplm_B = datos_Fig_Supplm_B_fit(5:4:end, 3);

datos_Fig_Supplm_B_histogram = readmatrix('fig2_panel_D_histogram.csv', 'Delimiter',';', 'DecimalSeparator',',');
sesion_hist_Supplm_B = datos_Fig_Supplm_B_histogram(2:76, 2);
medias_hist_Dyn_NR_Supplm_B = datos_Fig_Supplm_B_histogram(2:76, 5);
medias_hist_Dyn_R_Supplm_B = datos_Fig_Supplm_B_histogram(77:151, 5);
medias_hist_Stat_NR_Supplm_B = datos_Fig_Supplm_B_histogram(152:226, 5);
medias_hist_Stat_R_Supplm_B = datos_Fig_Supplm_B_histogram(227:end, 5);

% ------- Data for Panel C (First Draft - survival curves) -------

datos_Fig_Supplm_C = readmatrix('fig2_panel_E.csv', 'Delimiter',';', 'DecimalSeparator',',');
sesion_surv_Supplm_C = datos_Fig_Supplm_C(2:4:end, 2);

media_surv_Dyn_NR_Supplm_C = datos_Fig_Supplm_C(2:4:end, 3);
media_surv_Stat_NR_Supplm_C = datos_Fig_Supplm_C(3:4:end, 3);
media_surv_Dyn_R_Supplm_C = datos_Fig_Supplm_C(4:4:end, 3);
media_surv_Stat_R_Supplm_C = datos_Fig_Supplm_C(5:4:end, 3);

conf_low_Dyn_NR_Supplm_C = datos_Fig_Supplm_C(2:4:end, 4);
conf_low_Stat_NR_Supplm_C = datos_Fig_Supplm_C(3:4:end, 4);
conf_low_Dyn_R_Supplm_C = datos_Fig_Supplm_C(4:4:end, 4);
conf_low_Stat_R_Supplm_C = datos_Fig_Supplm_C(5:4:end, 4);

conf_high_Dyn_NR_Supplm_C = datos_Fig_Supplm_C(2:4:end, 5);
conf_high_Stat_NR_Supplm_C = datos_Fig_Supplm_C(3:4:end, 5);
conf_high_Dyn_R_Supplm_C = datos_Fig_Supplm_C(4:4:end, 5);
conf_high_Stat_R_Supplm_C = datos_Fig_Supplm_C(5:4:end, 5);

%% Ploteamos
clc

color=[[255, 0, 0];[255, 138, 0];[0, 9, 255];[10, 11, 140]]/255;

font_size_axis = 16;
font_size_labels = 20;
font_size_asterix = 20;

ancho_inset = 0.15;
alto_inset = 0.31;

altura_entre_paneles = 0.1;
ancho_entre_paneles = 0.1;

dist_hasta_Abajo = 0.17;
dist_hasta_Arriba = 0.07;

dist_hasta_Izquierda = 0.06;
dist_hasta_Derecha = 0.02;

subplot = @(m,n,p) subtightplot (m, n, p, [altura_entre_paneles ancho_entre_paneles], [dist_hasta_Abajo dist_hasta_Arriba], [dist_hasta_Izquierda dist_hasta_Derecha]);

figure('color', 'w', 'position', [50, 50, 870, 300]);

subplot(2, 15, [1:4, 16:19]);
hold on;
for i = 1:4
    errorbar(i, medias_Learn_Supplm_A(i), medias_Learn_Supplm_A(i)-error_low_Learn_Supplm_A(i), error_high_Learn_Supplm_A(i)-medias_Learn_Supplm_A(i), 'o','CapSize',11, 'LineWidth', 2, 'MarkerSize', 8, 'Color', color(i,:), 'MarkerEdgeColor', color(i,:), 'MarkerFaceColor', color(i,:))
end
set(gca, 'linewidth', 1, 'fontsize', font_size_axis)
xlh = xlabel('stimulus type', 'fontsize', font_size_labels);
xlh.Position(1) = xlh.Position(1);
xlh.Position(2) = xlh.Position(2)-0.012;
ylh = ylabel('lever-pressing prob', 'fontsize', font_size_labels);
ylh.Position(1) = ylh.Position(1)-0.8;
ylh.Position(2) = ylh.Position(2)+0.054;
xticks([])
xlim([0.5, 4.5]);
ylim([0, 0.6]);
text(0.23,0.82,'#','Units','normalized', 'fontsize', font_size_asterix, 'color', 'k')

% Histograms
subplot(2, 15, [5:10]);
axis off

% Dyn_Rew
axes('Position',[0.32 0.6 ancho_inset alto_inset])
hold on;
histogram('BinEdges',sesion_hist_Supplm_B','BinCounts',medias_hist_Dyn_R_Supplm_B(1:length(sesion_hist_Supplm_B)-1)', 'facecolor', color(1, :), 'edgecolor', 'w');
plot(sesion_fit_Supplm_B, medias_fit_Dyn_R_Supplm_B, 'color', color(1, :), 'LineWidth',3);
xlim([0, 15]);
ylim([0, 1.7e-4]);
xticks([])
set(gca, 'linewidth', 1, 'fontsize', font_size_axis)
ylh1 = ylabel('lever-pressing prob', 'fontsize', font_size_labels);
ylh1.Position(1) = ylh1.Position(1) - 0.5;
ylh1.Position(2) = ylh1.Position(2)-0.00011;
text(1.03,1.08,'#','Units','normalized', 'fontsize', font_size_asterix, 'color', 'k')

% Stat_Rew
axes('Position',[0.345+ancho_inset 0.6 ancho_inset alto_inset])
hold on;
histogram('BinEdges',sesion_hist_Supplm_B','BinCounts',medias_hist_Stat_R_Supplm_B(1:length(sesion_hist_Supplm_B)-1)', 'facecolor', color(2, :), 'edgecolor', 'w');
plot(sesion_fit_Supplm_B, medias_fit_Stat_R_Supplm_B, 'color', color(2, :), 'LineWidth',3);
xticks([])
yticks([])
xlim([0, 15]);
ylim([0, 1.7e-4]);
set(gca, 'linewidth', 1, 'fontsize', font_size_axis)

subplot(2, 15, [20:25]);
% axis off
ax = gca;
axis(ax,'off')
xlh = xlabel(ax,'time from stimulus onset (s)', 'fontsize', font_size_labels);
xlh.Position(1) = xlh.Position(1);
xlh.Position(2) = xlh.Position(2)-0.08;
ax.XLabel.Visible = 'on';
 
% Dyn_non-Rew
axes('Position',[0.32 0.17 ancho_inset alto_inset])
hold on;
histogram('BinEdges',sesion_hist_Supplm_B','BinCounts',medias_hist_Dyn_NR_Supplm_B(1:length(sesion_hist_Supplm_B)-1)', 'facecolor', color(3, :), 'edgecolor', 'w');
plot(sesion_fit_Supplm_B, medias_fit_Dyn_NR_Supplm_B, 'color', color(3, :), 'LineWidth',3);
xlim([0, 15]);
ylim([0, 1.7e-4]);
set(gca, 'linewidth', 1, 'fontsize', font_size_axis)

% Stat_non-Rew
axes('Position',[0.345+ancho_inset 0.17 ancho_inset alto_inset])
hold on;
histogram('BinEdges',sesion_hist_Supplm_B','BinCounts',medias_hist_Stat_NR_Supplm_B(1:length(sesion_hist_Supplm_B)-1)', 'facecolor', color(4, :), 'edgecolor', 'w');
plot(sesion_fit_Supplm_B, medias_fit_Stat_NR_Supplm_B, 'color', color(4, :), 'LineWidth',3);
yticks([])
xlim([0, 15]);
ylim([0, 1.7e-4]);
set(gca, 'linewidth', 1, 'fontsize', font_size_axis)

% survival curves
subplot(2, 15, [11:15, 26:30]);
hold on
fill([sesion_surv_Supplm_C; flipud(sesion_surv_Supplm_C)], [conf_low_Dyn_R_Supplm_C; flipud(conf_high_Dyn_R_Supplm_C)], color(1, :), 'EdgeColor', 'none','FaceAlpha',.25)
fill([sesion_surv_Supplm_C; flipud(sesion_surv_Supplm_C)], [conf_low_Stat_R_Supplm_C; flipud(conf_high_Stat_R_Supplm_C)], color(2, :), 'EdgeColor', 'none','FaceAlpha',.25)
fill([sesion_surv_Supplm_C; flipud(sesion_surv_Supplm_C)], [conf_low_Dyn_NR_Supplm_C; flipud(conf_high_Dyn_NR_Supplm_C)], color(3, :), 'EdgeColor', 'none','FaceAlpha',.25)
fill([sesion_surv_Supplm_C; flipud(sesion_surv_Supplm_C)], [conf_low_Stat_NR_Supplm_C; flipud(conf_high_Stat_NR_Supplm_C)], color(4, :), 'EdgeColor', 'none','FaceAlpha',.25)

p2 = plot(sesion_surv_Supplm_C, media_surv_Dyn_R_Supplm_C, 'LineWidth', 2, 'Color', color(1,:));
p1 = plot(sesion_surv_Supplm_C, media_surv_Stat_R_Supplm_C, 'LineWidth', 2, 'Color', color(2,:));
p4 = plot(sesion_surv_Supplm_C, media_surv_Dyn_NR_Supplm_C, 'LineWidth', 2, 'Color', color(3,:));
p3 = plot(sesion_surv_Supplm_C, media_surv_Stat_NR_Supplm_C, 'LineWidth', 2, 'Color', color(4,:));
set(gca, 'linewidth', 1, 'fontsize', font_size_axis)
xlabel('time from stimulus onset (s)', 'fontsize', font_size_labels);
ylh = ylabel('1st lever-pressing prob', 'fontsize', font_size_labels);
ylh.Position(1) = ylh.Position(1)-0.3;
ylh.Position(2) = ylh.Position(2);
text(0.15,0.85,'#','Units','normalized', 'fontsize', font_size_asterix, 'color', 'k')

















