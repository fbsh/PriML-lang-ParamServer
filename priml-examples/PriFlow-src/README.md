1. running asyncSGD demo in './async-demo' (an implementation of Hogwild!) 

```
docker build -t hogwild .
docker run -it --rm --name demo hogwild
```

1. running linear regression and neural network trainning task with SGD algorithm in './SGD'

```
sml lr.sml
sml nn.sml

```