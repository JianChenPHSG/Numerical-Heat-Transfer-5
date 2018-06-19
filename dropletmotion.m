%导入流场
clear,clc
load('u3.mat')
%初始化液滴参数
xd=0;yd=5e-3;
vf=zeros(2,2);
vd=zeros(2,2);
%vd(1,:)=ones(1,2)*5;
%利用插值确定液滴的流场值
F1=scatteredInterpolant(x,y,u,'linear');
F2=scatteredInterpolant(x,y,v,'linear');
vf(1,1)=3;
vf(2,1)=0;
vf(:,2)=vf(:,1);
dt=1e-5;
i=2;
xd(2)=xd(1);
yd(2)=yd(1);
FA=zeros(2,2);
FD=zeros(2,2);
while i<=100000
%利用四阶Rugge-Kutta
    
    dv1=acc(vf(:,i-1),vf(:,i),vd(:,i-1),vd(:,i),dt);
    
    vdtemp1=vd(:,i)+dv1*dt/2;
    xtemp1=xd(i)+vd(1,i)*dt/2+1/2*dv1(1)*(dt/2)^2;
    ytemp1=yd(i)+vd(2,i)*dt/2+1/2*dv1(2)*(dt/2)^2;
    vftemp1(1,1)=F1(xtemp1,ytemp1);
    vftemp1(2,1)=F2(xtemp1,ytemp1);
    dv2=acc(vf(:,i-1),vftemp1,vd(:,i-1),vdtemp1,dt/2);
    
    vdtemp2=vdtemp1+dv2*dt/2;
    xtemp2=xtemp1+vdtemp1(1)*dt/2+1/2*dv2(1)*(dt/2)^2;
    ytemp2=ytemp1+vdtemp1(2)*dt/2+1/2*dv2(2)*(dt/2)^2;
    vftemp2(1,1)=F1(xtemp2,ytemp2);
    vftemp2(2,1)=F2(xtemp2,ytemp2);
    dv3=acc(vftemp1,vftemp2,vdtemp1,vdtemp2,dt/2);
    
    vdtemp3=vdtemp2+dv3*dt;
    xtemp3=xtemp2+vdtemp2(1)*dt+1/2*dv3(1)*dt^2;
    ytemp3=ytemp2+vdtemp2(2)*dt+1/2*dv3(2)*dt^2;
    vftemp3(1,1)=F1(xtemp3,ytemp3);
    vftemp3(2,1)=F2(xtemp3,ytemp3);
    dv4=acc(vftemp2,vftemp3,vdtemp2,vdtemp3,dt);
    vd(:,i+1)=vd(:,i)+(dv1+2*dv2+2*dv3+dv4)/6*dt;
    xd(i+1)=xd(i)+dt/6*(vdtemp1(1)+vdtemp2(1)*2+vdtemp3(1)*2+vd(1,i+1));
    yd(i+1)=yd(i)+dt/6*(vdtemp1(2)+vdtemp2(2)*2+vdtemp3(2)*2+vd(2,i+1));
    vf1(1,1)=F1(xd(i),yd(i));
    vf1(2,1)=F2(xd(i),yd(i));
    vf2(1,1)=F1(xd(i+1),yd(i+1));
    vf2(2,1)=F2(xd(i+1),yd(i+1));
    [FD(:,i+1),FA(:,i+1)]=recordForce(vf1,vf2,vd(:,i),vd(:,i+1),dt);
    i=i+1;
    
    vf(1,i)=F1(xd(i),yd(i));
    vf(2,i)=F2(xd(i),yd(i));
    if xd(i)>=0&&xd(i)<1e-2
        if yd(i)<0
            yd(i)=-yd(i);
            %vd(2,i)=-vd(2,i);
        elseif yd(i)>1e-2
            yd(i)=2e-2-yd(i);
            %vd(2,i)=-vd(2,i);
        end
    elseif xd(i)>=1e-2&&xd(i)<=2e-2&&yd(i)<0
        yd(i)=-yd(i);
        %vd(2,i)=-vd(2,i);
    elseif yd(i)>=0&&yd(i)<1e-2&&xd(i)>2e-2
        xd(i)=4e-2-xd(i);
        %vd(1,i)=-vd(1,i);
    elseif yd(i)>=1e-2&&yd(i)<=2e-2
        if xd(i)>2e-2
            xd(i)=4e-2-xd(i);
            %vd(1,i)=-vd(1,i);
        elseif xd(i)<1e-2
            xd(i)=2e-2-xd(i);
            %vd(1,i)=-vd(1,i);
        end
    elseif yd(i)>2e-2
        break;
    end
   
end
%quiver(x,y,u,v)
%hold on
temp=find(xd>=0&xd<=2e-2&yd>=0&yd<=2e-2);
plot(xd(temp),yd(temp))