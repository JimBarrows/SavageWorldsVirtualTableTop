package org.savageworlds.game.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value="Dali", date="2014-12-18T06:47:28.017-0700")
@StaticMetamodel(HindranceDescription.class)
public class HindranceDescription_ {
	public static volatile SingularAttribute<HindranceDescription, Long> id;
	public static volatile SingularAttribute<HindranceDescription, String> name;
	public static volatile SingularAttribute<HindranceDescription, HindranceSeverity> severity;
	public static volatile SingularAttribute<HindranceDescription, String> effects;
}
