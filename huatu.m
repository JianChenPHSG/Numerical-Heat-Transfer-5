for k=1:numel(i)
xx(i(k),j(k))=x(i(k));
yy(i(k),j(k))=y(j(k));
uu(i(k),j(k))=u(k);
vv(i(k),j(k))=v(k);
end
uu(1,:)=3;
temp=find(xx<1e-2&yy>1e-2&yy<2e-2);
uu(temp)=NaN;
vv(temp)=NaN;
%mesh(xx,yy,sqrt(uu.^2+vv.^2))
%pcolor(xx,yy,sqrt(uu.^2+vv.^2));shading interp
%colorbar
TT=sqrt(uu.^2+vv.^2);
temp=find(yy>5e-3-1e-2/600&yy<5e-3+1e-2/600&xx>=0&xx<=1e-2);
plot(xx(temp),TT(temp))