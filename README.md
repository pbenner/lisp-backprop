## Structure

* src
  * Source code of the backpropagation algorithm

*data
  * Training and validation sets

* data/train
  * Data set generated from two images

* data/train2
  * Data set generated from ten images

* src/FourierMellin
  * MATLAB source code of the Fourier-Mellin transform

## How to generate the training set

Go to *data/train* or *data/train2* and execute *./import2.pl*
(edit the script to change the size of the data set).

Use *src/FourierMellin/applyCavanagh.m* to generate the raw and
invariant features for training (*raw.txt* and *features.txt*).

Use *data/tableto{lisp,svm}.pl* to convert the data set
to a format which can be read by lisp or the support vector
library:
```sh
../tabletolisp.pl < features.txt > features.lisp
```
The script *data/train2/scale.R* can be used to scale the
data appropriately.

## How to train a neural network
Load *'learning-algorithm.lisp'* into lisp and execute

```lisp
(defparameter features
  (make-instance 'learning-algorithm
                 :network (make-instance 'ff-ann :topology (list :layers '(1023 15 10) :transf *tanh*))
                 :data    (make-instance 'data :file "features.lisp" :percentage 10)
                 :algorithm 'backprop-incremental))
```

to create a neural network with 1023x15x10 neurons, tanh activation
function, where an incremental backprop algorithm is used. 10% of the
data set will be used as validation set.

Then execute
```lisp
(run features #'(lambda (a) (>= (epoch a) 200)) :learning-rate 0.001)
```
to train the network (the algorithm stops after 200 epochs).

To save/restore the weights of a network use:
```lisp
(save-weights    features "features.weights")
(restore-weights features "features.weights")
```
