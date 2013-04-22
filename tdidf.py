#!/usr/bin/env python

# Simple tf-idf examples

import math

## Test stuff

DOC  = "corpus/document.txt"
DOC2 = "corpus/document2.txt"

DOC_LIST = [DOC, DOC2]

class Tokenizer:
    @staticmethod
    def tokenize(document):
        def removeNonAscii(s):
            """ Remove non ascii chars from a string """
            return "".join(i for i in s if ord(i)<128)
        with open(document) as f:
            tokens = [t.lower() for t in f.read().split()]
            return map(lambda x: removeNonAscii(x), tokens)

class TfIdf:

    def __init__(self, documents=None):
        self.documents = documents

    def word_frequency(self, word, tokens):
        """ How many times does a word appear in a document? """
        return tokens.count(word)

    def document_term_frequency(self, term, document):
        tokens = Tokenizer.tokenize(document)
        return self.word_frequency(term, tokens)

    def tf(self, term, document):
        tokens = Tokenizer.tokenize(document)
        word_count  = len(tokens)
        term_occurs = self.word_frequency(term, tokens)
        return term_occurs / float(word_count)

    def docs_containing_term(self, term, documents):
        """ Returns the number of documents that contain a term """
        def term_occurs(term, document):
            count = self.document_term_frequency(term, document)
            if (count > 0): return 1
            else: return 0
        occurs = [term_occurs(term, d) for d in documents]
        return sum(occurs)

    def idf(self, term, documents):
        occurences = self.docs_containing_term(term, documents)
        return math.log(len(documents) / occurences)

    def tf_idf(self, word, document, documents):
        """ Returns the tf-idf score """
        return self.tf(word, document) * self.idf(word, documents)

