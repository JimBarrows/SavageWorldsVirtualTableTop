package org.savageworlds.game.model;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.validation.constraints.NotNull;
import javax.xml.bind.annotation.XmlRootElement;

@Entity
@XmlRootElement
public class EdgeDescription implements Serializable {

	/**
	 * 
	 */
	private static final long		serialVersionUID	= 1L;

	@Id
	@GeneratedValue
	private Long					id;

	@ManyToOne
	@NotNull
	private EdgeType				edgeType;

	@NotNull
	private String					name;

	private RankType				minimumRank			= RankType.Novice;

	private CharacterType			requiredType		= null;

	@OneToMany
	private Set<Skill>				minimumSkills		= new HashSet<Skill>();

	@OneToMany
	private Set<EdgeDescription>	requiredEdges		= new HashSet<EdgeDescription>();

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

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
