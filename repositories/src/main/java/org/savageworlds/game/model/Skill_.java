package org.savageworlds.game.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;
import jdo.model.BasePersistentModel_;

@Generated(value="Dali", date="2015-01-10T06:58:30.014-0700")
@StaticMetamodel(Skill.class)
public class Skill_ extends BasePersistentModel_ {
	public static volatile SingularAttribute<Skill, SkillDescription> skill;
	public static volatile SingularAttribute<Skill, DiceType> dice;
	public static volatile SingularAttribute<Skill, Integer> bonus;
	public static volatile SingularAttribute<Skill, EdgeDescription> edgeDescription;
}