#include "projectmanager.hpp"
#include "stringbuilder.hpp"

#include <llvm/ADT/SmallString.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Path.h>
#include <clang/Basic/Version.h>

std::vector<ProjectInfo> ProjectManager::systemProjects() {
    std::vector<ProjectInfo> projects;

    std::istringstream stream("include;/usr/include/");
    std::string        current;
    std::string        name;

    while (std::getline(stream, current, ';')) {
        if (name.empty()) {
            name = current;
        } else {
            projects.emplace_back(name, current, ProjectInfo::Internal);
            name.clear();
        }
    }

    return projects;
}


ProjectManager::ProjectManager() {
    for (auto&& info : systemProjects()) {
        addProject(info);
    }
}

bool ProjectManager::addProject(ProjectInfo info) {
    if (info.source_path.empty()) { return false; }
    llvm::SmallString<256> filename;
    llvm::sys::fs::real_path(info.source_path, filename);
    if (filename.empty()) { return false; }
    if (filename[filename.size() - 1] != '/') { filename += '/'; }
    info.source_path = filename.c_str();

    projects.push_back(std::move(info));
    return true;
}

ProjectInfo* ProjectManager::projectForFile(llvm::StringRef filename) {
    unsigned int match_length = 0;
    ProjectInfo* result       = nullptr;

    for (auto& it : projects) {
        const std::string& source_path = it.source_path;
        if (source_path.size() < match_length) { continue; }
        if (filename.startswith(source_path)) {
            result       = &it;
            match_length = source_path.size();
        }
    }
    return result;
}

std::string ProjectManager::includeRecovery(
    llvm::StringRef includeName,
    llvm::StringRef from) {

    if (includeRecoveryCache.empty()) {
        for (const auto& proj : projects) {
            // skip sub project
            llvm::StringRef sourcePath(proj.source_path);
            auto parentPath = sourcePath.substr(0, sourcePath.rfind('/'));
            if (projectForFile(parentPath)) { continue; }

            std::error_code EC;
            for (llvm::sys::fs::recursive_directory_iterator
                     it(sourcePath, EC),
                 DirEnd;
                 it != DirEnd && !EC;
                 it.increment(EC)) {
                auto fileName = llvm::sys::path::filename(it->path());
                if (fileName.startswith(".")) {
                    it.no_push();
                    continue;
                }
                includeRecoveryCache.insert(
                    {std::string(fileName), it->path()});
            }
        }
    }
    llvm::StringRef includeFileName = llvm::sys::path::filename(
        includeName);
    std::string resolved;
    int         weight = -1000;
    auto        range  = includeRecoveryCache.equal_range(
        std::string(includeFileName));
    for (auto it = range.first; it != range.second; ++it) {
        llvm::StringRef candidate(it->second);
        unsigned int    suf_len = 0;
        while (suf_len < std::min(candidate.size(), includeName.size())) {
            if (candidate[candidate.size() - suf_len - 1] !=
                includeName[includeName.size() - suf_len - 1]) {
                break;
            } else {
                suf_len++;
            }
        }
        // Each paths part that are similar from the expected name are
        // weighted 1000 points f
        int w = includeName.substr(includeName.size() - suf_len)
                    .count('/') *
                1000;
        if (w + 1000 < weight) { continue; }

        // after that, order by similarity with the from url
        unsigned int pref_len = 0;
        while (pref_len < std::min(candidate.size(), from.size())) {
            if (candidate[pref_len] != from[pref_len]) {
                break;
            } else {
                pref_len++;
            }
        }
        w += candidate.substr(0, pref_len).count('/') * 10;

        // and the smaller the path, the better
        w -= candidate.count('/');

        if (w < weight) { continue; }

        weight   = w;
        resolved = std::string(candidate);
    }
    return resolved;
}
