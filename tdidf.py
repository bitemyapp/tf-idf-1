#!/usr/bin/env python

# Simple tf-idf examples

import math

## Test stuff

DOC  = "corpus/document.txt"
DOC2 = "corpus/document2.txt"

DOC_LIST = [DOC, DOC2]

def removeNonAscii(s):
    """ Remove non ascii chars from a string """
    return "".join(i for i in s if ord(i)<128)

def tokenize(document):
    with open(document) as f:
        tokens = [t.lower() for t in f.read().split()]
        return map(lambda x: removeNonAscii(x), tokens)

def word_frequency(word, tokens):
    """ How many times does a word appear in a document? """
    return tokens.count(word)

def document_term_frequency(term, document):
    tokens = tokenize(document)
    return word_frequency(term, tokens)

def tf(term, document):
    tokens = tokenize(document)
    word_count  = len(tokens)
    term_occurs = word_frequency(term, tokens)
    return term_occurs / float(word_count)

def docs_containing_term(term, documents):
    """ Returns the number of documents that contain a term """
    def term_occurs(term, document):
      count = document_term_frequency(term, document)
      if (count > 0):
          return 1
      else: return 0
    occurs = [term_occurs(term, document) for document in documents]
    return sum(occurs)

def idf(term, documents):
    occurences = docs_containing_term(term, documents)
    return math.log(len(documents) / occurences)

def tf_idf(word, document, documents):
    """ Returns the tf-idf score """
    return tf(word, document) * idf(word, documents)

