package org.savageworlds.game.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value="Dali", date="2014-12-23T23:33:35.060-0700")
@StaticMetamodel(SkillDescription.class)
public class SkillDescription_ {
	public static volatile SingularAttribute<SkillDescription, Long> id;
	public static volatile SingularAttribute<SkillDescription, Long> version;
	public static volatile SingularAttribute<SkillDescription, String> name;
	public static volatile SingularAttribute<SkillDescription, AttributeTypes> attribute;
	public static volatile SingularAttribute<SkillDescription, String> description;
}
