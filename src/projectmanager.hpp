#pragma once

#include <llvm/ADT/StringRef.h>
#include <string>
#include <vector>
#include <unordered_map>

struct ProjectInfo {
    std::string name;
    std::string source_path;
    //    std::string description;
    //    std::string version_info;
    //    std::string repo_url; //may contains tags;
    std::string revision;

    std::string external_root_url;

    // TODO
    std::string fileRepoUrl(const std::string& file) const { return {}; }
    enum Type {
        Normal,
        Internal, // includes and stuffs
        External, // links to external projects somewhere else, do not
                  // generate refs or anything,
                  //  and link to a different ref source
    } type = Normal;

    ProjectInfo(std::string name, std::string source_path, Type t = Normal)
        : name(std::move(name))
        , source_path(std::move(source_path))
        , type(t) {}
    ProjectInfo(std::string name, std::string source_path, std::string rev)
        : name(std::move(name))
        , source_path(std::move(source_path))
        , revision(std::move(rev)) {}
};

struct ProjectManager {
    explicit ProjectManager();

    bool addProject(ProjectInfo info);

    std::vector<ProjectInfo> projects;

    /// \note the file name need to be canonicalized
    ProjectInfo* projectForFile(llvm::StringRef filename);

    std::string includeRecovery(
        llvm::StringRef includeName,
        llvm::StringRef from);

  private:
    static std::vector<ProjectInfo> systemProjects();

    std::unordered_multimap<std::string, std::string> includeRecoveryCache;
};
