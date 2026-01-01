# AGENTS.md

This file provides guidance to AI coding assistants when working with code in this repository.

## 概要

個人用 Emacs 設定リポジトリです。

## プロジェクト構成

以下2つのファイルから構成されている。

- init.el: メイン設定ファイル。基本的にはこちらに設定を記述する。
- early-init.el: 早期初期化ファイル。

## 設定方針

- 設定の記述は `use-package` を利用する
- パッケージのインストールには `straight.el` を用いる
  - 基本的に `use-package` と統合して用いる

## コマンド

- 設定ファイルが動作するかチェックする: `make check`
