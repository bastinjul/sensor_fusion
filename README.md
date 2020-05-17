# sensor_fusion
> Repository of the master thesis "sensor fusion" [@uclouvain](uclouvain.be) by Julien Bastin (julien.bastin@student.uclouvain.be) & Guillaume Neirinckx (guillaume.neirinckx@student.uclouvain.be)

## Deployment
The command to deploy on the grisp is : 

```
NAME=<hostname> [WIFI=true | ADHOC=true IP=X.X.X.X] rebar3 grisp deploy -n sensor_fusion -v 0.1.0
```