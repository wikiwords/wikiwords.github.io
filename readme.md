# WikiWords: A Wiktionary-based collection of linguistic resources

## About WikiWords
The aim of this project is to make data from <a href="https://www.wiktionary.org/" target="_blank">Wiktionary</a> available in a simple spreadsheet format as an easy-to-use resource for semi-automatic linguistic annotation. It is work-in-progress, and currently only a preliminary dataset of German nouns - containing information about their grammatical gender as well as a transcription in IPA - is available. 

Please note that the datasets made available here can contain all kinds of mistakes. Their purpose is to aid manual annotation, not to replace it. All results obtained using WikiWords should be checked carefully.

<img src="example.png" alt="Example" with="300" height="300">

WikiWords is a project by Peeter Tinits and Stefan Hartmann. All data are available under a CC-BY-SA-4.0 license. If you use WikiWords, please give attribution to the Wiktionary project. If you find the WikiWords database helpful, we'd be happy if you quoted it as:

Tinits, Peeter & Stefan Hartmann: WikiWords. A Wiktionary-based collection of linguistic resources. https://github.com/hartmast/WikiWords.

The WikiWords logo is based on the <a href="https://commons.wikimedia.org/wiki/File:WiktionaryEn.svg" target="_blank">Wiktionary logo</a> by Wikimedia Foundation, Inc., used and distributed here under a CC-BY-SA license.

<br />

## Datasets

Currently WikiWords contains the following datasets:

| Name          | Size (words)      | Size (MB)   | Language    | Based on dump from (date)
| ------------- |:-----------------:|:-----------:|:-----------:|----------------------------:|
| [WikiNounsDE](#wikinounsde)   | 72,063            | 3.8         | German      | 2019-03-20                  |



<br />

### WikiNounsDe

WikiNounsDe can be downloaded <a href="https://github.com/wikiwords/wikiwords.github.io/raw/master/wikinouns_DE.csv" target="_blank">here</a> (3.8 MB). It is a simple csv sheet containing the following columns:
* **Lexem**: the lexeme in question (nominative singular)
* **Genus**: the grammatical gender of the lexeme in question. (f = feminine, m = masculine, n = neuter; lexemes that can have more than one gender are tagged as fm, fn, mn, etc.)
* **IPA**: IPA transcription. In some cases, two transcriptions are given, separated by \|.
* **Worttrennung**: Syllabification. Note that the syllabification represents the orthographic separation of words, which does not always match the prosodic one.
* **Silbenzahl**: Number of syllables, automatically calculated from the syllabification column. <span style="color:#c52323"> **Caution**</span>: As mentioned above, the syllabification represents the orthographic word separation, which does not always match the prosodic one. For example, *Acker* is tagged as a one-syllable word because it is not usually split up in writing. Until a better solution is found, please check all entries in this column carefully when you use the database.

<br />

## Processing resources

### WikiNounsDe processing resources

The WikiNounsDe dataset was created from <a href="https://dumps.wikimedia.org/dewiktionary/20190301/dewiktionary-20190301-pages-articles.xml.bz2" target="_blank">this</a> Wiktionary dump using the R script wiktionary_process.R. To ensure that every word occurs only once, duplicate entries were merged in the columns "Worttrennung" and "IPA", separating various variants via \|.

<br />
<p align="center">
<a href="https://creativecommons.org/licenses/by-sa/4.0/">
<img src="by-sa.png" width="150">
</a>
</p>