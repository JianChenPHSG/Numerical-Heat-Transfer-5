function createfigure(ZData1, YData1, XData1, CData1, X1, Y1)
%CREATEFIGURE(ZData1, YData1, XData1, CData1, X1, Y1)
%  ZDATA1:  surface zdata
%  YDATA1:  surface ydata
%  XDATA1:  surface xdata
%  CDATA1:  surface cdata
%  X1:  x ���ݵ�����
%  Y1:  y ���ݵ�����

%  �� MATLAB �� 18-Jun-2018 19:35:49 �Զ�����

% ���� figure
figure1 = figure;

% ���� axes
axes1 = axes('Parent',figure1);
hold(axes1,'on');

% ���� surface
surface('Parent',axes1,'ZData',ZData1,'YData',YData1,'XData',XData1,...
    'DisplayName','data1',...
    'AlignVertexCenters','on',...
    'FaceColor','interp',...
    'EdgeColor','none',...
    'CData',CData1);

% ���� plot
plot(X1,Y1,'DisplayName','Һ���˶��켣','LineWidth',4,'Color',[1 0 0]);

% ȡ�������е�ע���Ա����������� X ��Χ
% xlim(axes1,[0 0.02]);
% ȡ�������е�ע���Ա����������� Y ��Χ
% ylim(axes1,[0 0.02]);
box(axes1,'on');
% ���� colorbar
colorbar('peer',axes1);

% ���� legend
legend1 = legend(axes1,'show');
set(legend1,...
    'Position',[0.15435475172841 0.730222206526332 0.168262650963383 0.0973333307902018]);

