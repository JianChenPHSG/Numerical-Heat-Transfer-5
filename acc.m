function [dvdt]=acc(vf1,vf2,vd1,vd2,dt)
%º∆À„“∫µŒ‘À∂Ø
%vf(1):uf;vf(2):vf
%droplet radius
g=9.8;
d=0.1e-3;
rhod=998;
mud=9.98e-4;
rhof=1.225;
muf=1.7894e-5;
md=pi*rhod*d^3/6;
Red=rhof*norm(vf2-vd2)*d/muf;
if Red==0
    CD0=10000;
else 
    CD0=24/Red*(Red<6.2&&Red>0)+10*Red^(-1/2)*(Red>=6.2&&Red<500)+...
    24/Red*(1+0.15*Red^0.687)*(Red>=500&&Red<800)+0.44*(Red>=800&&Red<2e5)+...
    0.1*(Red>2e5);
end
CD=CD0*(2*muf+3*mud)/(3*muf+3*mud);
FD=pi*CD*rhof*norm(vf2-vd2)*(vf2-vd2)*d^2/8;
FA=massgg(vf1,vf2,vd1,vd2,dt,rhof,d);
FV=pi*d^3*(rhod-rhof)*g/6;
temp=FD+FA+FV;
dvdt=temp/md;
