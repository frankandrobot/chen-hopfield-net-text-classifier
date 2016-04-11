# chen-hopfield-net-text-classifier

Based on "Automatic Concept Classification of Text from Electronic Meetings" by H. Chen and others.

Takes a set of documents and groups related terms into clusters.
The main limitations are:

1. if a term does not exist in the original input, the algorithm won't know what to do with it.
2. it is super slow. The running time to compute the weights is at least O(n^2).
   (However, once it calculates the weights, querying related terms is much quicker.)
3. getting reasonable results requires fine tuning the parameters---
   unfortunately, it is not clear how these should be tuned, 
   nor (and more importantly) is it clear that once tuned, the parameters
   will work for similar data sets.

## Running
Run `Main.scala`. It is pre-programmed to process several hundreds tweets
(see `resources/data`).
It outputs what it thinks are related terms for various input terms.
(See Main.scala)

## How it works

1. Extract terms (aka indexes)
```java
  def index(doc : String) = {

    List(doc)
      //first some preprocessing (like removing URLs)
      .map(_removeUrls)
      .map(_removeHashtagSymbol)
      .map(tokenizer.tokenize)
      //...
      //then given a list of tokens (using OpenNLP)
      //filter out stop words, digits, etc
      .filter(stopWords.isNotStopWord)
      .filter(_filterNonAlphanumeric)
      .filter(_filterDigits)
      .filter(_filterShortTokens)
      // then apply a stemmer
      .map(stemmer.stem)
  }
```
2. Generate a histogram of term frequency for each document
3. Iteratively eliminate documents whose terms don't appear enough until you've
   met a `docIndexingTarget`. This method is a way of reducing the sample space.
```java
@tailrec
final def infoLossAnalysis(docIndexingTarget : Double = 0.90,
                           docFrequencyThreshold : Int = 1,
                           prevDocs : List[Doc] = docStore.docs,
                           prevDiff : Double = 0) : List[Doc] = {

  val curDocs = prevDocs.filter( doc => {

    val terms = doc.terms.filter(docFrequency(_) >= docFrequencyThreshold)
    terms.length > 0
  })

  val ratio = curDocs.length.toDouble / docStore.docs.length.toDouble

  // This is decreasing, since you always remove more terms in each step
  val diff = ratio - docIndexingTarget

  if (diff <= 0) {
    if (prevDiff < diff) { return prevDocs }
    else { return curDocs }
  }

  return infoLossAnalysis(docIndexingTarget, docFrequencyThreshold + 1, curDocs, diff)
}
```
4. Define functions that do all the grunt work.
 - `termFrequency(doc, term)` = number of times the term occurs in the doc
 - `docFrequency(term)` = number of docs where the term occurs
 - `termFrequency(doc, term1, term2)` = number of times both terms occur in the doc
   i.e., `termFrequency(doc, term1) + termFrequency(doc, term2)`
 - `docFrequency(term1, term2)` = number of docs where both terms occur.

 *Notes:*

 -  We need `termFrequency(doc, term1, term2) <= termFrequency(doc, term1)`.
    Therefore, we define `termFrequency(doc, term1, term2) = min[termFrequency(doc, term1), termFrequency(doc, term2)]`.
    (Unfortunately, this is unclear in Chen's original paper.)
 -  `docFrequency(term1, term2) <= docFrequency(term1)`
 -  if term1 = term2, then
 ```
 termFrequency(term1, term2) = termFrequency(term1)
 docFrequency(term1, term2) = docFrequency(term1)
 ```
 -  It is always true that:
 ```
 termFrequency(term1, term2) = termFrequency(term2, term1)
 docFrequency(term1, term2) = docFrequency(term2, term1)
 ```
5. Define more functions to help calculate the weights:
 - `weight(doc, term) = termFrequency(doc, term) * log10(docFrequency(term))`
 - `weight(doc, term1, term2) = termFrequency(doc, term1, term2) * log10(docFrequency(term1, term2))`

 *Notes*:

 -  Since we're taking logs, these functions are undefined when the doc frequencies are 0. In `#infoLossAnalysis`, we eliminate low scoring terms so this isn't a problem for `#weight(doc, term)`. However, it does become a problem for the other weight function. See below for details.
 -  `weight(doc, term1, term2) <= weight(doc, term1)`.
    Therefore, `weight(doc, term1, term2) / weight(doc, term1) <= 1`
6. Define the weights (aka cluster weights):
 - `W(j, k) = sum[weight(doc, term_j, term_k)] / sum[weight(doc, term_j)]` where the sums are over all the docs

 *Notes*:

 Chen's original algorithm doesn't specify what happens when `#weight(doc, term1, term2)` is undefined. Since log(x) approaches negative infinity from the right, and points with negative weights are unrelated in a hopfield net, it makes sense to make `W(j, k)` = 0 whenever the numerator returns an undefined value (alternatively, setting it to negative infinity would also work)
