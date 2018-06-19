function FA=massgg(vf1,vf2,vd1,vd2,dt,rhof,d)
FA=pi*rhof*d^3*(vf2-vd2-(vf1-vd1))/dt/12;