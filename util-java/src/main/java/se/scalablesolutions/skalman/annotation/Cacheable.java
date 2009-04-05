/**
 * Copyright (C) 2009 Scalable Solutions.
 */

package se.scalablesolutions.skalman.annotation;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface Cacheable {}