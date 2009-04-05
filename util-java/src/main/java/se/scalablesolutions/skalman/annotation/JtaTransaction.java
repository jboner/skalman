/**
 * Copyright (C) 2009 Scalable Solutions.
 */

package se.scalablesolutions.skalman.annotation;

import java.lang.annotation.*;

public class JtaTransaction {
 
  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.METHOD)
  static public @interface REQUIRED {}

  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.METHOD)
  static public @interface REQUIRES_NEW {}

  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.METHOD)
  static public @interface SUPPORTS {}

  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.METHOD)
  static public @interface MANDATORY {}

  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.METHOD)
  static public @interface NEVER {}

  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.METHOD)
  static public @interface NOT_SUPPORTED {}
}