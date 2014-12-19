package org.savageworlds.game.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value="Dali", date="2014-12-18T06:47:28.026-0700")
@StaticMetamodel(Skill.class)
public class Skill_ {
	public static volatile SingularAttribute<Skill, Long> id;
	public static volatile SingularAttribute<Skill, SkillDescription> skill;
	public static volatile SingularAttribute<Skill, DiceType> dice;
	public static volatile SingularAttribute<Skill, Integer> bonus;
}
