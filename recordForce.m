function [FD,FA]=recordForce(vf1,vf2,vd1,vd2,dt)

d=0.1e-3;
rhod=998;
mud=9.98e-4;
rhof=1.225;
muf=1.7894e-5;
Red=rhof*norm(vf2-vd2)*d/muf;
CD0=24/Red*(Red<6.2)+10*Red^(-1/2)*(Red>=6.2&&Red<500)+...
    24/Red*(1+0.15*Red^0.687)*(Red>=500&&Red<800)+0.44*(Red>=800&&Red<2e5)+...
    0.1*(Red>2e5);
CD=CD0*(2*muf+3*mud)/(3*muf+3*mud);
FD=pi*CD*rhof*norm(vf2-vd2)*(vf2-vd2)*d^2/8;
FA=massgg(vf1,vf2,vd1,vd2,dt,rhof,d);