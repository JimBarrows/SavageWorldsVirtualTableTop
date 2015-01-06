package org.savageworlds.game.model;

import java.util.HashSet;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.validation.constraints.NotNull;
import javax.xml.bind.annotation.XmlRootElement;

import jdo.model.BasePersistentModel;

import org.hibernate.validator.constraints.NotEmpty;

@Entity
@XmlRootElement
public class EdgeDescription extends BasePersistentModel {

	/**
	 * 
	 */
	private static final long			serialVersionUID	= 1L;

	@ManyToOne
	@NotNull
	private EdgeType							edgeType;

	@NotEmpty
	private String								name;

	private RankType							minimumRank				= RankType.Novice;

	private CharacterType					requiredType			= CharacterType.Extra;

	@OneToMany(cascade=CascadeType.ALL)
	private Set<Skill>						minimumSkills			= new HashSet<Skill>();

	@OneToMany(cascade=CascadeType.MERGE)
	private Set<EdgeDescription>	requiredEdges			= new HashSet<EdgeDescription>();

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public RankType getMinimumRank() {
		return minimumRank;
	}

	public void setMinimumRank(RankType minimumRank) {
		this.minimumRank = minimumRank;
	}

	public CharacterType getRequiredType() {
		return requiredType;
	}

	public void setRequiredType(CharacterType requiredType) {
		this.requiredType = requiredType;
	}

	public Set<Skill> getMinimumSkills() {
		return minimumSkills;
	}

	public void setMinimumSkills(Set<Skill> minimumSkills) {
		this.minimumSkills = minimumSkills;
	}

	public Set<EdgeDescription> getRequiredEdges() {
		return requiredEdges;
	}

	public void setRequiredEdges(Set<EdgeDescription> requiredEdges) {
		this.requiredEdges = requiredEdges;
	}

	public EdgeType getEdgeType() {
		return edgeType;
	}

	public void setEdgeType(EdgeType edgeType) {
		this.edgeType = edgeType;
	}

}
