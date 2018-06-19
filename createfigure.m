function createfigure(ZData1, YData1, XData1, CData1, X1, Y1)
%CREATEFIGURE(ZData1, YData1, XData1, CData1, X1, Y1)
%  ZDATA1:  surface zdata
%  YDATA1:  surface ydata
%  XDATA1:  surface xdata
%  CDATA1:  surface cdata
%  X1:  x 数据的向量
%  Y1:  y 数据的向量

%  由 MATLAB 于 18-Jun-2018 19:35:49 自动生成

% 创建 figure
figure1 = figure;

% 创建 axes
axes1 = axes('Parent',figure1);
hold(axes1,'on');

% 创建 surface
surface('Parent',axes1,'ZData',ZData1,'YData',YData1,'XData',XData1,...
    'DisplayName','data1',...
    'AlignVertexCenters','on',...
    'FaceColor','interp',...
    'EdgeColor','none',...
    'CData',CData1);

% 创建 plot
plot(X1,Y1,'DisplayName','液滴运动轨迹','LineWidth',4,'Color',[1 0 0]);

% 取消以下行的注释以保留坐标区的 X 范围
% xlim(axes1,[0 0.02]);
% 取消以下行的注释以保留坐标区的 Y 范围
% ylim(axes1,[0 0.02]);
box(axes1,'on');
% 创建 colorbar
colorbar('peer',axes1);

% 创建 legend
legend1 = legend(axes1,'show');
set(legend1,...
    'Position',[0.15435475172841 0.730222206526332 0.168262650963383 0.0973333307902018]);

