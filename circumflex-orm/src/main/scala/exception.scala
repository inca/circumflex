package ru.circumflex.orm

class ORMException(msg: String) extends Exception(msg)

class ORMInitializationError(msg: String) extends Exception(msg)