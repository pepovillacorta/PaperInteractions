
%% %%%%%%%%%%%%%%%%%%%%%%%%   PANELS A-C  %%%%%%%%%%%%%%%%%%%%%%%%

clear all
clc

% %% Leemos datos

% ¡ATENCIÓN! En los .csv NO hay que tener en cuenta el 'std_error'; SÓLO
% los 'conf.low' y 'conf_high', que son los intervalos del std_error PERO
% TRANSFORMADOS debio a la regresión logística.

% --------- Data for Panel A (First Draft) -------------

datos_Fig_2A = readmatrix('fig2_panel_A.csv', 'Delimiter',';', 'DecimalSeparator',',');

sesion_surv_A = datos_Fig_2A(2:2:199, 2);
media_Dyn_R_A = datos_Fig_2A(2:2:199, 3);
media_Stat_R_A = datos_Fig_2A(3:2:199, 3);
conf_low_Dyn_R_A = datos_Fig_2A(2:2:199, 5);
conf_low_Stat_R_A = datos_Fig_2A(3:2:199, 5);
conf_high_Dyn_R_A = datos_Fig_2A(2:2:199, 6);
conf_high_Stat_R_A = datos_Fig_2A(3:2:199, 6);

media_Dyn_NR_A = datos_Fig_2A(200:2:end, 3);
media_Stat_NR_A = datos_Fig_2A(201:2:end, 3);
conf_low_Dyn_NR_A = datos_Fig_2A(200:2:end, 5);
conf_low_Stat_NR_A = datos_Fig_2A(201:2:end, 5);
conf_high_Dyn_NR_A = datos_Fig_2A(200:2:end, 6);
conf_high_Stat_NR_A = datos_Fig_2A(201:2:end, 6);

%% Plot lever-pressing prob

clc

color=[[255, 0, 0];[255, 138, 0];[0, 9, 255];[10, 11, 140]]/255;

font_size_axis = 16;
font_size_labels = 18;
font_size_asterix = 40;

altura_entre_paneles = 0.1;
ancho_entre_paneles = 0.1;

dist_hasta_Abajo = 0.25;
dist_hasta_Arriba = 0.07;

dist_hasta_Izquierda = 0.17;
dist_hasta_Derecha = 0.05;

subplot = @(m,n,p) subtightplot (m, n, p, [altura_entre_paneles ancho_entre_paneles], [dist_hasta_Abajo dist_hasta_Arriba], [dist_hasta_Izquierda dist_hasta_Derecha]);

figure('color', 'w', 'position', [50, 50, 400, 300]);
subplot(1, 1, 1);
hold on
fill([sesion_surv_A; flipud(sesion_surv_A)], [conf_low_Stat_R_A; flipud(conf_high_Stat_R_A)], color(2, :), 'EdgeColor', 'none','FaceAlpha',.1)
fill([sesion_surv_A; flipud(sesion_surv_A)], [conf_low_Dyn_R_A; flipud(conf_high_Dyn_R_A)], color(1, :), 'EdgeColor', 'none','FaceAlpha',.1)
fill([sesion_surv_A; flipud(sesion_surv_A)], [conf_low_Stat_R_A; flipud(conf_high_Stat_R_A)], color(2, :), 'EdgeColor', 'none','FaceAlpha',.1)
fill([sesion_surv_A; flipud(sesion_surv_A)], [conf_low_Dyn_R_A; flipud(conf_high_Dyn_R_A)], color(1, :), 'EdgeColor', 'none','FaceAlpha',.1)
fill([sesion_surv_A; flipud(sesion_surv_A)], [conf_low_Stat_NR_A; flipud(conf_high_Stat_NR_A)], color(4, :), 'EdgeColor', 'none','FaceAlpha',.1)
fill([sesion_surv_A; flipud(sesion_surv_A)], [conf_low_Dyn_NR_A; flipud(conf_high_Dyn_NR_A)], color(3, :), 'EdgeColor', 'none','FaceAlpha',.1)
fill([sesion_surv_A; flipud(sesion_surv_A)], [conf_low_Stat_NR_A; flipud(conf_high_Stat_NR_A)], color(4, :), 'EdgeColor', 'none','FaceAlpha',.1)
fill([sesion_surv_A; flipud(sesion_surv_A)], [conf_low_Dyn_NR_A; flipud(conf_high_Dyn_NR_A)], color(3, :), 'EdgeColor', 'none','FaceAlpha',.1)
p1 = plot(sesion_surv_A, media_Stat_R_A, 'LineWidth', 2.5, 'Color', color(2,:));
p2 = plot(sesion_surv_A, media_Dyn_R_A, 'LineWidth', 2.5, 'Color', color(1,:));
p3 = plot(sesion_surv_A, media_Stat_NR_A, 'LineWidth', 2.5, 'Color', color(4,:));
p4 = plot(sesion_surv_A, media_Dyn_NR_A, 'LineWidth', 2.5, 'Color', color(3,:));
set(gca, 'linewidth', 1, 'fontsize', font_size_axis)
xlabel({'normalized time'; '(0 = first session, 1 = last session)'}, 'fontsize', font_size_labels);
ylh = ylabel('lever-pressing prob', 'fontsize', font_size_labels);
ylh.Position(1) = ylh.Position(1)+0.003;
ylh.Position(2) = ylh.Position(2);
yticks([0, 0.1, 0.2, 0.3])
text(0.4,0.63,'*','Units','normalized', 'fontsize', font_size_asterix, 'color', color(1,:))

%% --------- Data for Panel B (First Draft) -------------

datos_Fig_2B = readmatrix('fig2_panel_B.csv', 'Delimiter',';', 'DecimalSeparator',',');

sesion_surv_B = datos_Fig_2B(2:70, 2);
media_Stat_NR_B = datos_Fig_2B(2:70, 3);
media_Dyn_NR_B = datos_Fig_2B(71:139, 3);
media_Stat_R_B = datos_Fig_2B(140:208, 3);
media_Dyn_R_B = datos_Fig_2B(209:end, 3);

conf_low_Stat_NR_B = datos_Fig_2B(2:70, 5);
conf_low_Dyn_NR_B = datos_Fig_2B(71:139, 5);
conf_low_Stat_R_B = datos_Fig_2B(140:208, 5);
conf_low_Dyn_R_B = datos_Fig_2B(209:end, 5);

conf_high_Stat_NR_B = datos_Fig_2B(2:70, 6);
conf_high_Dyn_NR_B = datos_Fig_2B(71:139, 6);
conf_high_Stat_R_B = datos_Fig_2B(140:208, 6);
conf_high_Dyn_R_B = datos_Fig_2B(209:end, 6);

%% Plot hazard

clc

color=[[255, 0, 0];[255, 138, 0];[0, 9, 255];[10, 11, 140]]/255;

font_size_axis = 16;
font_size_labels = 18;
font_size_asterix = 40;

altura_entre_paneles = 0.1;
ancho_entre_paneles = 0.1;

dist_hasta_Abajo = 0.25;
dist_hasta_Arriba = 0.07;

dist_hasta_Izquierda = 0.17;
dist_hasta_Derecha = 0.05;

subplot = @(m,n,p) subtightplot (m, n, p, [altura_entre_paneles ancho_entre_paneles], [dist_hasta_Abajo dist_hasta_Arriba], [dist_hasta_Izquierda dist_hasta_Derecha]);

figure('color', 'w', 'position', [50, 50, 400, 300]);
subplot(1, 1, 1);
hold on
fill([sesion_surv_B; flipud(sesion_surv_B)], [conf_low_Stat_R_B; flipud(conf_high_Stat_R_B)], color(2, :), 'EdgeColor', 'none','FaceAlpha',.1)
fill([sesion_surv_B; flipud(sesion_surv_B)], [conf_low_Dyn_R_B; flipud(conf_high_Dyn_R_B)], color(1, :), 'EdgeColor', 'none','FaceAlpha',.1)
fill([sesion_surv_B; flipud(sesion_surv_B)], [conf_low_Stat_R_B; flipud(conf_high_Stat_R_B)], color(2, :), 'EdgeColor', 'none','FaceAlpha',.1)
fill([sesion_surv_B; flipud(sesion_surv_B)], [conf_low_Dyn_R_B; flipud(conf_high_Dyn_R_B)], color(1, :), 'EdgeColor', 'none','FaceAlpha',.1)
fill([sesion_surv_B; flipud(sesion_surv_B)], [conf_low_Stat_NR_B; flipud(conf_high_Stat_NR_B)], color(4, :), 'EdgeColor', 'none','FaceAlpha',.1)
fill([sesion_surv_B; flipud(sesion_surv_B)], [conf_low_Dyn_NR_B; flipud(conf_high_Dyn_NR_B)], color(3, :), 'EdgeColor', 'none','FaceAlpha',.1)
fill([sesion_surv_B; flipud(sesion_surv_B)], [conf_low_Stat_NR_B; flipud(conf_high_Stat_NR_B)], color(4, :), 'EdgeColor', 'none','FaceAlpha',.1)
fill([sesion_surv_B; flipud(sesion_surv_B)], [conf_low_Dyn_NR_B; flipud(conf_high_Dyn_NR_B)], color(3, :), 'EdgeColor', 'none','FaceAlpha',.1)
p1 = plot(sesion_surv_B, media_Stat_R_B, 'LineWidth', 2.5, 'Color', color(2,:));
p2 = plot(sesion_surv_B, media_Dyn_R_B, 'LineWidth', 2.5, 'Color', color(1,:));
p3 = plot(sesion_surv_B, media_Stat_NR_B, 'LineWidth', 2.5, 'Color', color(4,:));
p4 = plot(sesion_surv_B, media_Dyn_NR_B, 'LineWidth', 2.5, 'Color', color(3,:));
set(gca, 'linewidth', 1, 'fontsize', font_size_axis)
xlabel({'normalized time'; '(0 = first session, 1 = last session)'}, 'fontsize', font_size_labels);
ylh = ylabel('hazard', 'fontsize', font_size_labels);
ylh.Position(1) = ylh.Position(1)-0.02;
ylh.Position(2) = ylh.Position(2);
text(0.4,0.7,'*','Units','normalized', 'fontsize', font_size_asterix, 'color', color(1,:))
















