#!/usr/bin/env python3

# Simple tf-idf examples

import math

class Tokenizer:
    @staticmethod
    def tokenize(document):
        def removeNonAscii(s):
            """ Remove non ascii chars from a string """
            return "".join(i for i in s if ord(i)<128)
        with open(document, 'r', encoding="utf-8") as f:
            tokens = [t.lower() for t in f.read().split()]
            result = map(lambda x: removeNonAscii(x), tokens)
            return list(result)

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

    def normalized_term_frequency(self, tf): pass

    def docs_containing_term(self, term, documents):
        """ Returns the number of documents that contain a term """
        def term_occurs(term, document):
            count = self.document_term_frequency(term, document)
            if (count > 0): return 1
            else: return 0
        occurs = [term_occurs(term, d) for d in documents]
        return sum(occurs)

    def idf(self, term, documents):
        "General importance of the term across document collection"
        occurences = self.docs_containing_term(term, documents)
        return math.log(len(documents) / 1 + occurences)

    def tf_idf(self, word, document, documents):
        """ Returns the tf-idf score. A high weight of the tf-idf calculation
            is reached when you have a high term frequency (tf) in the given document (local parameter)
            and a low document frequency of the term in the whole collection (global parameter) """
        return self.tf(word, document) * self.idf(word, documents)

# Cosine similarity

def dot_product(v1, v2):
    "Dot product for two vectors"
    return sum(x*y for x,y in zip(v1, v2))

def norm(a):
    n = len(a)
    for i in xrange(n):
        sum += a[i] * a[i]
    return math.sqrt(sum)

def cosine_similarity(a, b):
    return dot(a,b) / (norm(a) * norm(b))

def main():

    doc1  = "corpus/document.txt"
    doc2  = "corpus/document2.txt"
    doc3  = "corpus/document3.txt"
    doc_list = [doc1, doc2, doc3]

    tfidf = TfIdf()

    TERM = "catabolic"

    term_found                 = tfidf.docs_containing_term(TERM, doc_list)
    term_frequency             = tfidf.tf(TERM, doc3)
    inverse_document_frequency = tfidf.idf(TERM, doc_list)

    print(term_found)
    print(term_frequency)
    print(inverse_document_frequency)

    print(tfidf.tf_idf(TERM, doc2, doc_list))

if __name__ == '__main__': main()

