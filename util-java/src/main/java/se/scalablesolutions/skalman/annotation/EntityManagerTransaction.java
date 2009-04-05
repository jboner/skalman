/**
 * Copyright (C) 2009 Scalable Solutions.
 */

package se.scalablesolutions.skalman.annotation;

import java.lang.annotation.*;

public class EntityManagerTransaction {
 
  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.METHOD)
  static public @interface READ {}

  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.METHOD)
  static public @interface WRITE {}
}