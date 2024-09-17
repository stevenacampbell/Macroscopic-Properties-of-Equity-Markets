# Code supplement for ``Macroscopic Properties of Equity Markets: Stylized Facts and Portfolio Performance''

This repository contains all of the code to reproduce the analysis in [Campbell, Song & Wong (2024)]. Each section in the paper has a corresponding code folder that generates the figures and results. The market data used in the study was obtained under license from the Center for Research in Security Prices through Wharton Research Data Services (WRDS). Academic licenses are available at many institutions and the data can be accessed directly or through an API. For a tutorial on how to download and manipulate the data in Python see https://github.com/johruf/CRSP_on_WRDS_introduction.

Included in this repository is a pre-processing folder whose code files can be used to clean and format the CRSP data. We also provide a backtesting engine for long-only portfolios in markets with dividends, delistings, and transaction costs that implements the methodology of [Ruf & Xie (2020)]. This is used in Section 6 of the paper for an empirical study of portfolio performance. We include a dedicated folder for this backtesting engine that contains the main backtesting functionality and additional code to: 

i) Generate sample data in a format analogous to that of the paper,
ii) ``Pre-process'' the sample data for backtesting, and;
iii) Apply the backtesting engine to analyze trading performance on the sample data.
